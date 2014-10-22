##
## CIPS-R Source Code
## A program that runs Center for Intensive Planted for Silviculture (CIPS) Tools in R
## Developed by the Center for Intensive Planted-Forest Silviculture (CIPS)
## Version 2.0.0 Last edit: 10/10/2014
## Author: Nathaniel L. Osborne, Ph.D. Student 
## Support: Nathaniel.Osborne@oregonstate.edu
##

## Function to call when CIPS-R is loaded
.OnAttach <- function(){
	
	# Introduction text and documentation
	introtxt = "CIPS-R is an interface to Organon and Cipsanon developed at CIPS: Version 2.0"
	packageStartupMessage(introtxt)
	
	# Required packages indicated in Imports section of namespace
	#suppressMessages(lapply(c("XLConnect","raster"), require, character.only=T))
	
	# Load and mask cipsr example data (maybe not necessary)!
	#data("cipsrexam", package="cipsr")
	
}

.OnAttach() # Run procedures after loading the program

## Function imports formatted Excel databases
load.data <- function( InputFile ) {
		
	## Ensure that the user-specified database exists in the working directory
	if(!file.exists(InputFile)) {return(winDialog("ok","Input Database Does Not Exist."))}
	
	## Ensure that the user-specified database is in the correct file format
	if(!grepl(".xls",InputFile)) {return(winDialog("ok","Reformat Database to a .xls Extension"))}
	
	## Load the database provided XLConnect is loaded, the database exists and it is formatted correctly 
	InputList = list(samples=readWorksheetFromFile(InputFile, sheet="samples"),
			units=readWorksheetFromFile(InputFile, sheet="units"),
			activities=readWorksheetFromFile(InputFile, sheet="activities"))
	
	## Ensure that the user formatted all tabs correctly; if not the InputList will not exist
	if(!exists("InputList")) {return(winDialog("ok","Database Tabs Are Not Named Correctly."))}
	
	return( InputList ) # Return the loaded inputlist
}

## Function contains the thinning and fertilization algorithms for Organon/Cipsanon
.treatment <- function(executed,activity,triggers) {
	
	indicators = rep(0,2) # Treatment indicators [1] thinning, [2] fertilization; keys to what CIPSR should update
	inform = list(indicators,NA,NA) # Initilize a list to reinform Organon with
	if(nrow(activity)==0) return(inform) # If no activities are specified for the sample, return blank list
	
	## Evalulate if any activities should be triggered for the subperiod (1) using subperiod (0) information
	trigger = matrix(ncol=6,nrow=nrow(activity),dimnames=list(1:nrow(activity),names(triggers)))
	trigger[,2:6] = sapply(subset(names(triggers),names(triggers)!="year"),function(x){activity$trigger==x & triggers[[x]]>=activity$when}) # Evaluate greater than equal to statements
	trigger[,1] = triggers$year==activity$when
	
	## Check for input that may generate fatal situations
	fatal = 0 # Initialize to zero
	fatal[length(unique(executed$stage[executed$subperiod==1]))==5] <- 1 # Cannot handle more than five thinnings
	fatal[triggers$tpa<=50] <- 1 # Do not allow thinnings, where trees per acre below or at 50 TPA
	
	## If any triggering conditions were met, apply the user-specified treatment for subperiod (1)
	if(any(trigger) & fatal==0) {
		
		## Extract the orders to act upon from the triggers
		act = lapply(names(triggers), function(x) {
					if(x=="year") {
						activity[activity$trigger==x & triggers[[x]]==activity$when,]
					} else {
						activity[activity$trigger==x & triggers[[x]]>=activity$when,]
					}
				}
		)
		act = do.call("rbind",act) # Combine all orders into a single list
		act = merge(activity,act,sort=F) # Preserve user-defined order; first orders take priority		
		act = rbind(act[which(act$what=="thin"),][1,],act[which(act$what=="fert"),][1,]) # In the case of multiple orders, take just the first order  
		act = act[complete.cases(act),] # Prohibit consideration of incomplete cases
		
		## * Begin the Thinning Algorithm * ##
		if(any(act$what=="thin")) {
			
			## Reduce the executed treelist to only the most recent iteration and begin building the output object
			# * Note that, expansion factors have already been divided by number of points in the input treelist object!
			out = subset(executed,period==max(period,na.rm=TRUE) & subperiod==0) 
			out$mgexp = out$expan # Cut tree expansion factor *! to be reduced during thinning
			out$subperiod = 1 # The subperiod becomes equal to one
			
			## Extract user-supplied arguments to the thinning algorithm
			how = act$how[act$what=="thin"] # Method of thinning
			metric = act$metric[act$what=="thin"] # Metric associated with the target condition
			target = as.numeric(act$target[act$what=="thin"]) # Target condition in units of the metric
			target = round(target,1) # Only accept one decimal accuracy; convert if necessary
			
			## Assess feasibility of the proposed treatment	
			fatal = 0 # Presume no fatal errors
			# A special variant of CIPS-R allows for 'row below thinning'; if existing in activities list test for common errors
			if("level" %in% names(act)){
				level = act$level[act$how=="rowbelow"] # Level of row below thinning to be applied		
				fatal[how=="rowbelow" & !(metric %in% c("bap","rel"))] <- 1 # Row below thinning only supports a basal area target for the residual stand
				fatal[how=="rowbelow" & level<=0 | level>=1] <- 1 # Row thinning level must be a ratio between 0 and 1
			}
			# If row-below thinning specified, but no level argument existing, correct template not in use!
			if(how=="rowbelow" & !("level" %in% names(act))) {fatal<-1; winDialog("ok","Need Level Argument to be Supplied")}
			fatal[triggers$tpa==0] <- 1  # Trees per acre is zero
			fatal[metric=="tpa" & triggers$tpa < target] <- 1 # Target trees per acre greater than current trees per acre
			fatal[metric=="bap" & triggers$bap < target] <- 1 # Target basal area per acre greater than current basal area per acre
			fatal[metric=="sdi" & triggers$sdi < target] <- 1 # Target stand density index greater than current stand density index
			fatal[metric=="rel" & triggers$rel < target] <- 1 # Target relative density greater than current relative density
			fatal[target < 0] <- 1 # The target sample condition was negative
			fatal[how=="user" & metric!="prop"] <- 1 # User thin requested with out a proportion metric specified
			fatal[how=="user" & !triggers$year %in% out$user] <- 1 # User thinning without the year indicator specified
			if(fatal==1) winDialog("ok",paste("Infeasible Thinning:","Unit",unique(activity$unit))) # Report a fatal error if it occurs
			
			# Provided no fatal errors, procede into the thinning algorithms
			if(fatal==0){
				## Switch through a series of methods for thinning and apply the relevant approach
				switch(how,
						# Uniform thinning; all trees treated equal
						"uniform"={
							
							## If simple metrics supplied, calculate using a ratio
							if(metric%in%c("prop","tpa","bap")){
								r = 0 # Amount to remove, initialized to zero
								r[metric=="prop"] <- target 
								r[metric=="tpa"] <- 1 - (target/triggers$tpa)   
								r[metric=="bap"] <- (triggers$bap-target) / triggers$bap 
								out$expan = out$expan - (r*out$expan) # Update expansion factors from thinning
								out$mgexp = out$mgexp-out$expan # Reduce to find cut tree expansion factor
								
							} else {
								
								## If more complex metrics supplies, calculate with iterative reduction approach
								
								## If stand density metric supplied, iteratively reduce until the correct target reached
								if(metric=="sdi"){
									bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
									qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
									sdi = sum(out$expan)*(qmd/10)^1.605 # Stand density index
									repeat{
										out$expan=out$expan-0.001 # Remove expansion factor 
										out$expan[out$expan<0] = 0 # No expansion factor may be below zero
										bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
										qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
										sdi = sum(out$expan)*(qmd/10)^1.605 # Stand density index
										if(all.equal(0,target-sdi,tolerance=0.1)==TRUE | sdi<target){break}  # Break when within three decimals of precise relative density
									}
								}
								
								## If relative density metric supplied, iteratively reduce until the correct target reached
								if(metric=="rel"){
									bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
									qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
									rel = bap/(qmd)^0.5 # Relative density, as calculated by Curtis
									repeat{
										out$expan=out$expan-0.001 # Remove expansion factor 
										out$expan[out$expan<0] = 0 # No expansion factor may be below zero
										bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
										qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
										rel = bap / (qmd)^0.5  # Relative density, as calculated by Curtis
										if(all.equal(0,target-rel,tolerance=0.1)==TRUE | rel<target){break}  # Break when within four decimals of precise relative density
										
									}
								}
							}	
						},
						"below"={
							## Sort tree list, provide index for assurances
							out$index = 1:nrow(out) # Assign an index for 'assured' order
							out$ba = pi*(out$dbh/2)^2/144*out$expan # Calculate basal area per acre for each tree
							out = out[order(out$ba),] # Sort the tree list by diameter breast height (in) 		
							
							## If simple metrics supplied, calculate using a ratio
							if(metric%in%c("prop","tpa","bap")){
								r = 0 # Amount to remove, initialized to zero
								r[metric=="prop"] <- target * triggers$bap # Basal area to remove
								r[metric=="bap"] <- triggers$bap-target
								r[metric=="tpa"] <- triggers$tpa-target # Trees to remove   
								p = NA # The proportion of the expansion factor to remove as r goes to zero 
								
								## Application of thinning for basal area per acre and proportional targets
								if(metric=="bap" | metric=="prop") {     
									for(j in 1:nrow(out)) {
										p[r>=out$ba[j]] <- 1
										p[r<out$ba[j]] <- r/out$ba[j]
										reduce=out$ba[j]*p
										out$expan[j] <- out$expan[j]-out$expan[j]*p  
										r = r-reduce   
									}
								}
								
								## Application of thinning for tree per acre, stand and relative density targets
								if(metric=="tpa") {
									for(j in 1:nrow(out)) {
										p[r>=out$expan[j]] <- 1
										p[r<out$expan[j]] <- r/out$expan[j]
										reduce = out$expan[j]*p
										out$expan[j] <- out$expan[j]-out$expan[j]*p  
										r = r-reduce   
									}
								}
								
							} else {
								
								## If more complex metrics supplies, calculate with iterative reduction approach
								if(metric%in%c("sdi","rel")){
									
									## If stand density metric supplied, reduce iteratively until target met
									if(metric=="sdi"){
										## Go in reverse order through the treelist, calculating RD to identify point at which removal must be specific
										for(i in which(out$expan>0)){
											bap = sum(pi*(out$dbh[1:i]/2)^2/144*out$expan[1:i]) # Basal area per acre
											qmd = sqrt(bap/(0.005454*sum(out$expan[1:i]))) # Calculate quadratic mean diameter
											sdi = sum(out$expan[1:i])*(qmd/10)^1.605 # Stand density index
											# True/False statement; if target is nearly reached
											if(sdi>(triggers$sdi-target)){break} # Identify break point to finely reduce the expansion factor
										}
										if(i>1){out$expan[1:(i-1)] <- 0} # Trees below k+1 point removed completed if k > 1	
										
										## Remove expansion factor until target reached
										repeat{
											out$expan[i]=out$expan[i]-0.001 # Remove expansion factor 
											out$expan[out$expan<0] = 0 # No expansion factor may be below zero
											bap = sum(pi*(out$dbh/2)^2/144*out$expan) # Basal area per acre
											qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
											sdi = sum(out$expan)*(qmd/10)^1.605 # Stand density index
											if(all.equal(0,target-sdi,tolerance=0.01)==TRUE | sdi<target){break} # Break when within three decimals of precise relative density
										}
									}
									
									## If relative density metric supplied, reduce iteratively until target met
									if(metric=="rel"){
										## Go in reverse order through the treelist, calculating RD to identify point at which removal must be specific
										for(i in which(out$expan>0)){
											bap = sum(pi*(out$dbh[1:i]/2)^2/144*out$expan[1:i]) # Basal area per acre
											qmd = sqrt(bap/(0.005454*sum(out$expan[1:i]))) # Calculate quadratic mean diameter
											rel = bap/(qmd)^0.5 # Calculate relative density, under Curtis' definition
											# True/False statement; if target is nearly reached
											if(rel>(triggers$rel-target)){break} # Identify break point to finely reduce the expansion factor
										}
										if(i>1){out$expan[1:(i-1)] <- 0} # Trees below k point removed completed if k > 1	
										repeat{
											## Remove expansion factor until target reached
											out$expan[i]=out$expan[i]-0.001 # Remove expansion factor 
											out$expan[out$expan<0] = 0 # No expansion factor may be below zero
											bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
											qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
											rel = bap / (qmd)^0.5  # Relative density, as calculated by Curtis
											if(all.equal(0,target-rel,tolerance=0.1)==TRUE | rel<target){break} # Break when within three decimals of precise relative density
										}
									}							
								}
							}
							
							## Without respect to metric employed, finalize expansion factors in the same way
							out$mgexp = out$mgexp-out$expan # Reduce to find cut tree expansion factor
							out = out[order(out$index),] # Return to the orginal order
							out = subset(out,select=-c(ba,index)) # Drop the basal area per tree and index columns
						},
						"rowbelow"={
							
							## Provided spreadsheet formatted for 'special procedure' apply row-below thinning (McTague and Osborne, 2013)
							out$index = 1:nrow(out) # Assign an index value to ensure good sorting
							out$dc = findInterval(out$dbh,seq(1,240,by=2))*2 # Bin into 2'' DBH (in) classes
							out$bap = pi*(out$dbh/2)^2 /144 * out$expan # Calculate basal area each tree represents (sq ft/ac)
							divide = aggregate(list(bap=out$bap),by=list(dc=out$dc),FUN=sum) # Calculate basal area by 2'' diameter class
							divide = divide$dc[which.max(divide$bap)] # Identify maximum basal area for a given 2'' diameter class (division point)
							
							## [1] First cut trees uniformly; by row thinning given a user specified level
							pr = 1-level # Proportion to remove by row thinning 
							out$expan = pr*out$expan # Row thin trees at the given level; reducing expansion factors
							
							## [2] Second, cut all trees from the smallest diameter class
							smallcut = out[out$expan!=0,] # Identify trees with expansion factors
							smallcut = smallcut$index[smallcut$dc==min(smallcut$dc)] # Position (index) of the smallest trees
							out$expan[smallcut] <- 0 # Set smallest trees expansion factors to zero         
							
							## [3] Third, cut 65% of the trees below the divide and 35% of the trees above the divide; dependent of metric employed
							if(metric=="bap"){
								
								## Check for fatal errors, then proceed into routine
								bap = sum(with(out,pi*(dbh/2)^2/144*(expan)))  # Calculate basal area per acre (ft2/ac)
								fatal[target>=bap] <-1 # Fatal error; Not enough basal area left to meet target
								
								## Provided enough basal area exists, continue procedure
								if(fatal==0){
									r = 0.65*(bap-target) # Identify basal remove from below
									out$bap = pi*(out$dbh/2)^2 /144 * out$expan # Calculate basal area each tree represents (sq ft/ac)
									below = out[out$dc<divide,] # Tree's from below the mode
									above = out[out$dc>=divide,] # Tree's from above the mode
									
									# If not possible to take 65% basal area from below, zero out all trees from below
									if(r>sum(below$bap)){
										below$expan=0 # Complete zero out expansion factors  
									} else {
										# Remove 65% of basal area from below
										below$expan = r/sum(below$bap) * below$expan # Reduce expansion factors by the cut ratio (r/B)
									}
									
									# Take remaining basal area from above	
									out = rbind(above,below) # Recombine above and below split
									bap = sum(with(out,pi*(dbh/2)^2/144*(expan))) # Recalculate total basal area per acre (ft2/ac)
									r = bap-target # Update cut factor, for the above trees
									out$expan[out$dc>=divide] <- with(out[out$dc>=divide,],expan-r/sum(bap)*expan) # Remove remaining basal area from above
								}
								
							} else {
								
								## Check for fatal errors, then proceed into routine
								bap = with(out,sum(dbh^2*0.005454*expan)) # Calculate basal area per acre (ft2/ac)
								qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate Dq (in)
								rel = bap/(qmd)^0.5 # Calculate relative density, under Curtis' definition
								fatal[metric!="rel"] <- 1 # Fatal error; Something other than RD specified
								fatal[target>=rel] <-1 # Fatal error; Not enough RD left to meet target
								
								if(fatal==0){
									r = 0.65*(rel-target) # Identify percent of RD to remove from below
									below = out[out$dc<divide,] # Tree's from below the mode
									above = out[out$dc>=divide,] # Tree's from above the mode
									
									# Take 65% of RD from below, if possible; evaluate initial conditions to determine feasibility
									bap = sum(pi*(below$dbh/2)^2/144*(below$expan)) # Basal area per acre
									qmd = sqrt(bap/(0.005454*sum(below$expan))) # Calculate quadratic mean diameter
									rel = bap / (qmd)^0.5  # Relative density, as calculated by Curtis
									
									# If not possible to take 65% RD from below, zero out all trees from below
									if(r>rel){
										r=r-rel # Reduce r, the amount of relative density to remove
										below$expan=0 # Complete zero out expansion factors if 
									} else {
										# If possible to take 65% of RD from below, do so iteratively until target reached
										repeat{
											below$expan=below$expan-0.001 # Remove expansion factors, until r reduced to zero
											below$expan[below$expan<0] = 0 # No expansion factor may be below zero
											bap = sum(pi*(below$dbh/2)^2/144*(below$expan)) # Basal area per acre
											qmd = sqrt(bap/(0.005454*sum(below$expan))) # Calculate quadratic mean diameter
											r = r - (rel-(bap/(qmd)^0.5)) # Reduce r, by the prior calculated RD (i.e. Find the RD removed)
											if(all.equal(0,r,tolerance=0.01)==TRUE) {break} # Break when approximate removal target reached
											rel = bap/(qmd)^0.5  # Calculate relative density, as calculated by Curtis
										}
									}
									## Remove remaining RD from above
									out = rbind(above,below) # Recombine above and below split
									bap = with(out,sum(dbh^2*0.005454*expan)) # Calculate basal area per acre (ft2/ac)
									qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate Dq (in)
									rel = bap/(qmd)^0.5 # Calculate relative density, under Curtis' definition
									repeat{
										out$expan[out$dc>=divide]=out$expan[out$dc>=divide]-0.001
										out$expan[out$expan<0] = 0 # No expansion factor may be below zero
										bap = sum(pi*(out$dbh/2)^2/144*(out$expan)) # Basal area per acre
										qmd = sqrt(bap/(0.005454*sum(out$expan))) # Calculate quadratic mean diameter
										rel = bap/(qmd)^0.5 # Relative density, as calculated by Curtis
										if(all.equal(1,target/rel,tolerance=0.01)==TRUE) {break} # Break when approximate removal target reached
									}	
									
								}
							}
							
							out = out[order(out$index),] # Reorder by the index
							out = subset(out,select=-c(index,dc,bap)) # Drop columns used to the thinning procedure
							out$mgexp = out$mgexp-out$expan # Find the cut tree expansion factor; see the earlier procedures
							
						},
						"user"={
							out$expan[out$user==triggers$year] <- out$expan[out$user==triggers$year] * (1-target)
							out$mgexp = out$mgexp-out$expan # Reduce to find cut tree expansion factor
						}
				)
			}
			
			inform[[1]][1][fatal==0] <- 1 # Indicate thinning, given no fatal errors
			inform[[2]] = out # Submit the thinned tree list
		}
		
		## * Begin the Fertilization Algorithm * ##
		if(any(act$what=="fert")) {
			
			## Extract user-supplied arguments to the fertilization algorithm
			how = act$how[act$what=="fert"] # Method of thinning
			metric = act$metric[act$what=="fert"] # Metric associated with the target condition
			target = as.numeric(act$target[act$what=="fert"]) # Target condition in units of the metric
			out = target # Send out pound of nitrogen applied 
			
			inform[[1]][2] <- 1 # Indicate fertilization, given no fatal errors
			inform[[3]] = out # Submit the fertilization output 
			
		}	
	}
	
	return(inform) # Return information to inform Organon, based on indicator keys
}

## Function runs Organon and CIPSANON in R  
cipsr <- function( InputList ) {
	
	## Perform Program Start up tasks
	
	# Identify directories used when running the program
	home = getwd() # User workspace
	root = path.package("cipsr") # Root directory for the cipsr package 
	dlls = paste(root,"src",sep="/") # Relative to root; location of dll files
	spat = paste(root,"inst/spat",sep="/") # Relative to root; location of spatial files
	warn = paste(root,"inst/warn",sep="/") # Relative to root; location of dll files
	
	## Identify home for creating outputs - (with relative location to where package has been installed) loading dlls - loading spatial files 
	
	## Ensure that 32-bit version of R is running, else DLL files cannot be loaded
	if("i386"!=as.character(version[2])) { return(winDialog("ok","Only R i386 is Supported"))}
	## Ensure that the input list contains the correct set of tables
	if(!all(c("samples","units","activities") %in% names(InputList))) { return(winDialog("ok","Input List Is Not Correctly Formatted"))} 
	## To avoid importing null values, limit input tables to complete entries
	samples = InputList$samples[complete.cases(InputList$samples),]
	units = InputList$units[complete.cases(InputList$units),]
	activities = InputList$activities[complete.cases(InputList$activities),]
	
	## Ensure that user supplied unit index values match across tables; otherwise stop
	index = list(samples$unit,units$unit,activities$unit)
	if(length(index[[3]])==0) {index[[3]] <- NULL}	# Omit activities from the check if none are supplied
	nomatch = NA  # In a combinatorial way, check all table indices match up and record results as true or false	
	for(j in 1:length(index)) {nomatch[j]=!all(sapply(1:2, function(x) index[[j]] %in% index[[x]]))}
	if(any(nomatch)) {return(winDialog("ok","At Least One Sample and Unit Index Combination Does Not Match Between the Input Tables"))}
	
	rm(InputList,index,nomatch,j); invisible(gc()) # Clear out duplicate objects and flush out memory
	
	## Ensure that the user did not supply data with obvious errors that will cause fatal CIPS-R to crash
	if(any(units$variant==2 & units$woodqual==1)) {return(winDialog("ok","Currently, Cipsanon Does Not Support Simulation of Wood Quality, But It Will Very Soon!"))}  
	if(any(subset(samples,select=-c(unit,sample,tree))  < 0)) {return(winDialog("ok","At Least One Negative Measurement Value Exists"))}
	if(any(units$variant==2 & units$version==2 & units$driver==1)) {return(winDialog("ok","SMC Version of CIPSANON Does Not Support a Mechanistic Site Index In This Version of ONR"))}
	if(any(units$variant==2 & units$version==3)) {return(winDialog("ok","CIPSANON Only Supports Versions One and Two"))}  
	if(any(units$variant==2 & units$version==2 & units$driver==1)) {return(winDialog("ok","CIPSANON Supports a Mechanistic Site Index Only for Version One"))}
	if(any(units$variant==2 & units$dfsi==0 & units$otsi==0 & units$woodqual==1)) {return(winDialog("ok","Wood Quality Estimation Only Possible When Traditional Site Index Estimates Are Supplied"))}
	if(any(units$groyrs==0)){return(winDialog("ok","At Least One Unit Has Grow Years Set To Zero"))}
	
	## If the user plans to run the statistical version of Organon with climate and soils, prepare rasters
	if(any(units$driver==1)) {	
			
		## Now load soil and weather coverages
		rasters <- new.env() # Set up a raster enviornment to call
		assign('whc50',raster(paste(spat,"whc50.img",sep="/")),envir=rasters) # Water holding capacity from 0 - 50 cm (%)
		assign('pptdd5',raster(paste(spat,"pptdd5.img",sep="/")),envir=rasters) # Growing season precipitation (mm)
	}
	
	## Create and initialize a progress bar to track processing of samples
	T = length(unique(units$unit)) + 1 # The total number of orders to tick by
	pbar <- winProgressBar(title="CIPS-R Progress: 0%",min=0,max=T,width=300) 
	
	## Function to run the statistical variant of Organon (CIPSANON) for a single sample
	.cipsanon <- function( sample, unit, activity ) {
		
		n = nrow(sample) # The number of trees included in the 'big sample'; a combination of many samples
		
		## Errors can occur when running Organon; load the possible error table
		flags = read.csv(paste(warn,"flags.csv",sep="/")) # Read the table into R
		flags = split(flags,flags$level) # Split up by stand and tree level errors
		flags$stand$exists = 0 # Assume that stand level errors do not exist 
		## Reformat tree level errors given sample size dimensions assuming no errors exist
		flags$tree = as.data.frame(matrix(0,nrow=n,ncol=17,dimnames=list(1:n,unique(flags$tree$flag))))
		
		## Extract soil and climatic estimates to drive productivity if specified for the sample 
		DFSQ = 0 # Initialize indicator for running the mechanistic model to zero
		extracts = rep(0,2) # Initialize extracted values to zero
		# Initialize extracted soil and weather attributes to zero
		if( unit$driver==1 ) {				
			# Find the sample location using the supplied latitude and longitude
			p <- SpatialPoints( cbind(unit$longitude,unit$latitude),proj4string=CRS("+proj=longlat") ) 
			extracts = c(unit$whc,unit$pptdd) # Set by default, extracted values to what the user specified
			## If the user left WHC and PPTDD blank, give the a 'CIPS best guess'
			extracts[1][extracts[1]==0] = extract(get('whc50',envir=rasters),p)/100 * 19.685 # Convert WHC from a percentage to a ratio, and to inches by multiplying by 20 (50 cm = 19.685 inches)
			extracts[2][extracts[2]==0] = extract(get('pptdd5',envir=rasters),p) * 0.0393701 # Convert PPTDD to inches 
			DFSQ[all(!is.na(extracts))] <- 1 # Provided all drivers found, indicate to run the mechanistic model
		}
		extracts[DFSQ==0] <- 0 # Zero out the extracted values if they will not be used to drive productivity		
		# Report if the sample could not be driven with the mechanistic model
		if(unit$driver==1 & DFSQ==0) {winDialog("ok",paste("Could Not Drive Unit",unit$unit,"Sample",unit$sample,"With the Mechanistic Model, a Traditional Site Index Was Used Instead"))}
		
		## Format Organon vectors for use in the Fortran subroutine (prepare) 
		SPECIES=rep(0,2000);USER=rep(0,2000);DBH=rep(0,2000);HT=rep(0,2000);CR=rep(0,2000);EXPAN=rep(0,2000);RADGRO=rep(0,2000);IERROR=0;
		RVARS=rep(0,30);SERROR=rep(0,13);TERROR=rep(0,2000*6);SWARNING=rep(0,8);TWARNING=rep(0,2000);GROWTH=rep(0,2000);ACALIB=rep(0,3*18);
		
		## Supply arguments to each prepare subroutine vector
		VERSION = unit$version # Version of Organon to run
		NPTS = length(unique(sample$sample)) # Number of sample points fixed at one
		NTREES = n # Number of trees in the 'big' sample
		STAGE = unit$stage # Stand age
		BHAGE = unit$bhage # Breast height age
		SPECIES[1:n] = sample$species # Species for each tree
		USER[1:n] = sample$user # Code for user thinning 
		IEVEN = unit$iseven # If the stand is even aged or not
		DBH[1:n] = sample$dbh # Diameter at breast height for each tree (in)
		HT[1:n] = sample$tht # Total height for each tree (ft)
		CR[1:n] = sample$cr # Crown ratio for each tree 
		EXPAN[1:n] = sample$expan # Expansion factor for each tree (tpa)
		RADGRO[1:n] = sample$radgro # User supplied estimates of radial growth (in)
		RVARS[1:7] = c(unit$dfsi,unit$otsi,unit$dfsdi,unit$wgsdi,unit$phsdi,extracts[1],extracts[2]) # Set of seven indicator variables
		IRAD = if(any(RADGRO > 0)) {1} else {0} # Indicator if radial growth measurements were entered
		
		## Call the prepare subroutine in CIPSEDIT.dll to impute missing values in the sample
		dyn.load(paste(dlls,"CIPSEDIT.dll",sep="/"),type="Fortran") # Prepare subroutine
		prepared = .Fortran("prepare",as.integer(VERSION),as.integer(NPTS),as.integer(NTREES),as.integer(STAGE),
				as.integer(BHAGE),as.integer(SPECIES),as.integer(USER),as.integer(IEVEN),as.integer(DFSQ),as.single(DBH),
				as.single(HT), as.single(CR),as.single(EXPAN),as.single(RADGRO),as.single(RVARS),as.integer(SERROR), 
				as.integer(TERROR), as.integer(SWARNING),as.integer(TWARNING),as.integer(IERROR),as.integer(IRAD),
				as.single(GROWTH),as.single(ACALIB)) 
		dyn.unload(paste(dlls,"CIPSEDIT.dll",sep="/")) # Unload the prepare subroutine
		
		fatal = prepared[[20]] # Check if a fatal error occured while processing the sample
		if(fatal==1) {winDialog("ok",paste("Fatal Error Occured While Preparing Unit",unit$unit))} # Report if a fatal error occured during the prepare routine
		
		## Extract stand and tree level warnings and errors from the prepare subroutine
		flags$stand$exists[flags$stand$routine=="prepare"] <- c(prepared[[16]],prepared[[18]])
		for(z in 1:5) {flags$tree[,z] <- prepared[[17]][(1+(z-1)*2000):(2000*(z-1)+n)]}
		flags$tree[,7] <- prepared[[19]][1:n]
		
		## Provided no fatal errors, procedure to growing the stand
		if(fatal==0) {
			
			## Format Organon vectors for use in the Fortran subroutine (execute/wood quality)
			TREENO=rep(0,2000);PTNO=rep(0,2000);SPECIES=rep(0,2000);USER=rep(0,2000);INDS=rep(0,30);DBH1=rep(0,2000);HT1=rep(0,2000);
			CR1=rep(0,2000);SCR1=rep(0,2000);EXPAN1=rep(0,2000);MGEXP=rep(0,2000);RVARS=rep(0,30);ACALIB=rep(0,3*18);PN=rep(0,25);
			YSF=rep(0,25);BART=rep(0,25);YST=rep(0,25);NPR=rep(0,2000);PRAGE=rep(0,2000*3);PRLH=rep(0,2000*3);PRDBH=rep(0,2000*3);
			PRHT=rep(0,2000*3);PRCR=rep(0,2000*3);PREXP=rep(0,2000*3);BRCNT=rep(0,2000*3);BRHT=rep(0,2000*40);BRDIA=rep(0,2000*40);
			JCORE=rep(0,2000*40);SERROR=rep(0,35);TERROR=rep(0,2000*6);SWARNING=rep(0,9);TWARNING=rep(0,2000);DGRO=rep(0,2000);
			HGRO=rep(0,2000);CRCHNG=rep(0,2000);SCRCHNG=rep(0,2000);MORTEXP=rep(0,2000);NTREES2=rep(0,1);DBH2=rep(0,2000);HT2=rep(0,2000);
			CR2=rep(0,2000);SCR2=rep(0,2000);EXPAN2=rep(0,2000);STOR=rep(0,30);BABT=0;CYCLG=0;IDIB=rep(0,2000*40);
			
			## Supply arguments to each execute subroutine vector
			VERSION = prepared[[1]] # Version of Organon to run
			NPTS = prepared[[2]] # Number of sample points fixed at one
			NTREES1 = prepared[[3]] # Number of trees prior to growing
			STAGE = prepared[[4]] # Stand age
			BHAGE = prepared[[5]] # Breast height age 
			if(all(sample$tree==0)) {TREENO[1:n]=1:n} else {TREENO[1:n]=sample$tree} ## If all tree number set to zero, create tree numbers for the 'big sample'					
			PTNO[1:n] = sample$sample # Sample number corresponding to each tree
			SPECIES = prepared[[6]] # Species for each tree
			USER = prepared[[7]] # Code for user thinning
			# Set of 16 indicator variables
			INDS[1:16] = c(unit$dhcal,unit$ccal,unit$dgrocal,unit$iseven,unit$triple,0,unit$partcut,unit$pastfert,unit$maxsdi,unit$woodqual,0,0,0,unit$genes,unit$snc,DFSQ)
			DBH1 = prepared[[10]] # Diameter breast height for each tree (in), prior to growing   
			HT1 = prepared[[11]] # Total tree height for each tree (ft), prior to growing
			CR1 = prepared[[12]] # Crown ratio for each tree, prior to growing
			EXPAN1 = prepared[[13]] # Expansion factor for each tree (tpa), prior to growing
			RVARS[1:2][DFSQ!=1] <- c(unit$dfsi,unit$otsi) # If a mechanistic site index is user, leave blank.  Else, populate with normal site index values
			RVARS[3:10] = c(unit$dfsdi,unit$wgsdi,unit$phsdi,unit$gdval,unit$ghval,unit$dfret,extracts[1],extracts[2]) # Eight additional indicator values
			ACALIB = prepared[[23]] # Prepared calibration values
			
			# Supply arguments to each wood quality subroutine vector
			IJCALC = unit$core # Definition of the juvenile wood core
			IFINAL = 0 # Indicator for a final felling
			ACTION = 1 # Specify an initial fill in of wood quality values
			NINGRO = 0 # Number of ingrowth trees
			NTREES = NTREES1 # Number of trees
			NWQT = 0 # Internal value of wood quality DLL
			SITE_1 = unit$dfsi # Douglas-fir site index
			SITE_2 = unit$otsi # Other site index
			PDEN = 0 # Starting number of Red Alder trees per acre
			SCR = SCR1 # Shadow crown ratio
			
			## If user specified quality values, update vectors for wood quality
			if(unit$woodqual==1) {
				
				# If the wood quality routine is in use, it is necessary to update input with imputated values 
				CR = CR1 # Update crown ratio (in case of imputation)
				HT = HT1 # Update tree height (in case of imputation)
				
				dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
				glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
						as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
						as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
				dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the wood quality DLL
				
				BRCNT = glasslog[[26]] # Branch count
				BRDIA = glasslog[[27]] # Branch diameter
				BRHT = glasslog[[28]] # Branch height 
				JCORE = glasslog[[29]] # Juvenile core diameter
				IDIB = glasslog[[30]] # Inside bark diameter 
				NWQT = glasslog[[10]] # Internal variable
			}
			
			## Prior to growing, create a blank matrix to store output from growing the stand
			executed = matrix(ncol=14,nrow=(2000*unit$groyrs)) # Set matrix dimensions equal to the maximum size possible under the projection length
			## Specify the column names, and built a blank, 'big' tree list
			colnames(executed) <- c("sample","period","subperiod","stage","user","tree","species","dbh","tht","cr","expan","mgexp","cfv","bfv") 
			executed = as.data.frame(executed) # Convert to a data frame to facilitate referencing by column name
			## Insert the initial sample values prior to growing; Note that tree number is the user submitted identifier 
			executed[1:n,1:12] <- cbind(PTNO[1:n],rep(0,n),rep(0,n),rep(STAGE,n),USER[1:n],TREENO[1:n],SPECIES[1:n],DBH1[1:n],HT1[1:n],CR1[1:n],EXPAN1[1:n]/NPTS,MGEXP[1:n]/NPTS)
			rtab = n + 1 # Row tab to index the placement of new values during the growth procedure
					
			## Grow the sample to user-specified rotation age
			for(k in 1:unit$groyrs) {
				
				## Calculate values used to evaluate if a treatment should be applied each iteration
				triggers = data.frame(year=STAGE,tpa=sum(EXPAN1/NPTS),bap=sum(pi*(DBH1/2)^2/144*(EXPAN1/NPTS)))
				triggers$qmd = with(triggers,sqrt(bap/(0.005454*tpa)))
				triggers$sdi = with(triggers,tpa*(qmd/10)^1.605)
				triggers$rel = triggers$bap/(triggers$qmd^0.5) 
				
				## Evaluate if a treatment should be applied, and update if conditions are met
				inform = .treatment(executed,activity,triggers) 	
				
				## Inform the program about thinning treatments if they were made
				if(inform[[1]][1]==1) {	
					
					## Update elements associated with the execute subroutine
					executed = rbind(executed,inform[[2]]) # Attach new subperiod to the end of the treelist 
					BABT = triggers$bap # Basal area before thinning (ft2/acre)
					EXPAN1[1:NTREES1] = inform[[2]]$expan*NPTS # Unadjusted expansion factor for each tree (tpa)
					MGEXP[1:NTREES1] = inform[[2]]$mgexp*NPTS # Unadjusted expansion factor for cut trees (tpa)									
					BART[which(BART>0)+1] <- BART[which(BART>0)] # Slide all non-zero basal area removed values up a position
					BART[1] = BABT-sum(pi*(inform[[2]]$dbh/2)^2/144*inform[[2]]$expan) # Calculate basal area removed by thinning (ft2/ac)
					YST[which(YST>0)+1] <- YST[which(YST>0)] # Slide YST values up a position
					YST[1] = CYCLG # Set years since thinning (* in single year increments)
					INDS[13][any(SPECIES[MGEXP>0] %in% c(15,17,81,117,122,202,231,242,263))] <- 1 # Indicate if major conifers were cut
					INDS[11] <- 1 # Indicator that an overstory tree was harvested
					INDS[7] <- 1 # Indicator for partial cutting of the sample
					
					## Finalize wood quality for thinned trees
					ACTION = 4 # Set action to finalize thinned tree value
					NTREES = NTREES1 # Update the vectors for wood quality
					DBH = DBH1 # Diameter at breast height for each tree (in)
					HT = HT1 # Total height for each tree (ft)
					CR = CR1 # Crown ratio for each tree 
					SCR = SCR1 # Shadow crown ratio for each tree (tpa)
					EXPAN = EXPAN1 # Expansion factor for each tree (tpa)
					
					## If user specified quality values, update vectors for wood quality
					if(unit$woodqual==1) {
						dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
						## Calculate wood quality attributes for thinned trees
						glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
								as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
								as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
						dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the Organon wood quality subroutine
						
						## Update relevant wood quality vectors
						BRCNT = glasslog[[26]] # Branch count
						BRDIA = glasslog[[27]] # Branch diameter
						BRHT = glasslog[[28]] # Branch height 
						JCORE = glasslog[[29]] # Juvenile core diameter
						IDIB = glasslog[[30]] # Inside bark diameter 
						NWQT = glasslog[[10]] # Internal variable
					}
					
					## Capture wood quality values and associate with any thinned tree values if they exist				
					glassin = data.frame(sample=rep(PTNO,40),period=CYCLG,subperiod=1,stage=STAGE,tree=rep(TREENO,40),mgexp=rep(MGEXP/NPTS,40),brht=BRHT,brdia=BRDIA,jcore=JCORE,idib=IDIB)
					if(exists("glassout")) {glassout=rbind(glassout,glassin)} else {glassout=glassin} # Bind in, or name the wood quality output	
					
				}
				
				## If fertilization treatment was made, supply arguments to inform the execute subroutine 
				if(inform[[1]][2]==1) {	
					PN[which(PN>0)+1] <- PN[which(PN>0)] # Slide all non-zero values for pounds fertilized applied
					PN[1] = inform[[3]] # Update with pounds of fertilizer applied 
					YSF[which(YSF>0)+1] <- YSF[which(YSF>0)] # Slide all non-zero values for years since fertilization
					YSF[1] = CYCLG # Update the years since fertilization (* in single year increments)
					INDS[8] <- 1 # Indicate the stand has been fertilized
				}
				
				## Call the execute subroutine in CIPSRUN.dll to grow the sample
				dyn.load(paste(dlls,"CIPSRUN.dll",sep="/"), type="Fortran") # Load the execute subroutine 
				grow = .Fortran("execute",as.integer(CYCLG), as.integer(VERSION),as.integer(NPTS), as.integer(NTREES1), as.integer(STAGE), as.integer(BHAGE), 
						as.integer(TREENO), as.integer(PTNO), as.integer(SPECIES), as.integer(USER),as.integer(INDS), as.single(DBH1), as.single(HT1), as.single(CR1), 
						as.single(SCR1), as.single(EXPAN1), as.single(MGEXP),as.single(RVARS), as.single(ACALIB), as.single(PN), as.single(YSF),as.single(BABT), 
						as.single(BART), as.single(YST), as.integer(NPR),as.integer(PRAGE), as.single(PRLH), as.single(PRDBH), as.single(PRHT), as.single(PRCR), 
						as.single(PREXP), as.integer(BRCNT), as.integer(BRHT),as.integer(BRDIA), as.integer(JCORE), as.integer(SERROR), as.integer(TERROR), 
						as.integer(SWARNING), as.integer(TWARNING), as.integer(IERROR),as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG), 
						as.single(MORTEXP), as.integer(NTREES2), as.single(DBH2), as.single(HT2),as.single(CR2), as.single(SCR2), as.single(EXPAN2), as.single(STOR)) 
				dyn.unload(paste(dlls,"CIPSRUN.dll",sep="/")) # Unload the execute subroutine 
				
				## Update the values for the grown sample
				TREENO = grow[[7]] # Tree numbers
				PTNO = grow[[8]] # Sample number
				SPECIES = grow[[9]] # Tree species
				USER = grow[[10]] # Update the user code
				NTREES1 = grow[[46]] # Number of trees
				STAGE = grow[[5]] # Stand age
				BHAGE = grow[[6]] # Breast height age
				DBH1 = grow[[47]] # Diameter at breast height for each tree (in)
				HT1 = grow[[48]] # Total height for each tree (ft)
				CR1 = grow[[49]] # Crown ratio for each tree
				SCR1 = grow[[50]] # Shadow crown ratio for each tree
				EXPAN1 = grow[[51]] # Expansion factor for each tree (tpa)
				CRCHNG = grow[[43]] # Change in crown ratio for each tree
				SCRCHNG = grow[[44]] # Change in shadow crown ratio for each tree
				STOR = STOR[CYCLG==0] <- grow[[52]] # Obtain only on the first call  
				DGRO = grow[[41]] # Diameter growth rate (in)
				HGRO = grow[[42]] # Height growth rate (ft)
				
				fatal = grow[[40]] # Indicator for a fatal error occur while growing the stand
				if(fatal==1) {break()} # If a fatal error occured, break loop and do not accumulate any values 
				
				## Insert the updated tree values into the executed value table; As before, the user submitted tree number is assigned			
				executed[rtab:(rtab+NTREES1-1),1:12] <- cbind(PTNO[1:NTREES1],rep(k,NTREES1),rep(0,NTREES1),rep(STAGE,NTREES1),USER[1:NTREES1],TREENO[1:NTREES1],SPECIES[1:NTREES1],
						DBH1[1:NTREES1],HT1[1:NTREES1],	CR1[1:NTREES1],EXPAN1[1:NTREES1]/NPTS,MGEXP[1:NTREES1]/NPTS)
				
				## Update wood quality attributes from the grown tree information
				ACTION = 3 # By default, add whorls each cycle
				NTREES = NTREES1 # Number of trees
				DBH = DBH1 # Diameter at breast height for each tree (in)
				HT = HT1 # Total height for each tree (ft)
				CR = CR1 # Crown ratio for each tree 
				SCR = SCR1 # Shadow crown ratio for each tree
				EXPAN = EXPAN1 # Expansion factor for each tree (tpa)
				BRCNT = grow[[32]] # Branch count
				BRHT = grow[[33]] # Branch height
				BRDIA = grow[[34]] # Branch diameter
				JCORE =  grow[[35]] # Juvenile core 
				
				## If user specified quality values, update vectors for wood quality
				if(unit$woodqual==1 & inform[[1]][1]!=1 ) {
					dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
					## Call the wood quality subroutine in ORGWQ.dll to estimate wood quality
					glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
							as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
							as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
					dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the Organon wood quality subroutine
					
					BRCNT = glasslog[[26]] # Branch count
					BRDIA = glasslog[[27]] # Branch diameter
					BRHT = glasslog[[28]] # Branch height 
					JCORE = glasslog[[29]] # Juvenile core diameter
					IDIB = glasslog[[30]] # Inside bark diameter 
					NWQT = glasslog[[10]] # Internal variable
				}
				
				## Update iterative indexing values and restore some subroutine vectors
				rtab = rtab + NTREES1 # Update the row tab to index the placement of new values during the growth procedure
				MGEXP = rep(0,2000) # Expansion factor for cut trees
				INDS[11] = 0 # Reset indicator for cutting overstory trees
				INDS[13] = 0 # Reset indicator for cutting major conifer trees
				CYCLG = k # Update the value of cycles grown so far
			}	
			
			if(fatal==1) {winDialog("ok",paste("Fatal Error Occured While Growing Unit",unit$unit))} # Report if a fatal error occured during the growth routine
			
			executed = executed[!is.na(executed$period),] ## Recalling that maximum possible matrix space was added to place grown tree values, reduce to populated rows
			
			## Extract stand and tree level warnings and errors from the execute subroutine
			flags$stand$exists[flags$stand$routine=="execute"] <- c( grow[[36]],grow[[38]] ) 
			for( z in 1:6 ) { flags$tree[,z] <- grow[[37]][(1+(z-1)*2000):(2000*(z-1)+n)] }
			flags$tree[,7] <- grow[[39]][1:n]
		}
		
		## If a fatal error did not occur during growth, final-fell the forest 
		if(fatal==0) {
			## Input final-felling information as a subperiod (1)
			rtab = nrow(executed) + 1 # Update the row index value
			executed = rbind(executed,executed[executed$period==CYCLG & executed$subperiod==0,]) # Create space for the final-felling period
			executed[rtab:nrow(executed),]$subperiod <- 1 # Specify the subperiod as a final felling (1); Thinning not possible at final cycle
			executed[executed$period==CYCLG & executed$subperiod==1,]$mgexp <- executed[executed$period==CYCLG & executed$subperiod==1,]$expan # Update cut tree expansion factor
			executed[executed$period==CYCLG & executed$subperiod==1,]$expan <- 0 # No trees have an expansion factor after harvest
			
			## Calculate finalized wood quality values
			ACTION = 5 # Finalize wood quality values
			IFINAL = 1 # Indicate a final harvest
			EXPAN = rep(0,2000) # Expansion factor is set to zero
			MGEXP[1:NTREES1] = executed[rtab:nrow(executed),]$mgexp # Update the thinned tree expansion factor (tpa)
			
			## If user specified quality values, update vectors for wood quality
			if(unit$woodqual==1) {
				## Call the wood quality subroutine in ORGWQ.dll to estimate wood quality
				dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the wood quality DLL
				glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
						as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
						as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
				dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the wood quality subroutine
				
				BRCNT = glasslog[[26]] # Branch count
				BRDIA = glasslog[[27]] # Branch diameter
				BRHT = glasslog[[28]] # Branch height 
				JCORE = glasslog[[29]] # Juvenile core diameter
				IDIB = glasslog[[30]] # Inside bark diameter 
				NWQT = glasslog[[10]] # Internal variable
			}
			
			## Capture final-felling wood quality values and associate with any thinned tree values if they exist	
			glassin = data.frame(sample=rep(PTNO,40),period=CYCLG,subperiod=1,stage=STAGE,tree=rep(TREENO,40),mgexp=rep(MGEXP,40),brht=BRHT,brdia=BRDIA,jcore=JCORE,idib=IDIB)
			if(exists("glassout")) {glassout=rbind(glassout,glassin)} else {glassout=glassin} # Bind in, or name the wood quality output
			
			## Prepare the wood quality information for output		
			glassout = cbind(unit=unit$unit,glassout) # Assign the unit identifier 
			glassout$brht = glassout$brht/10 # Convert branch height (ft)
			glassout$brdia = glassout$brdia/100 # Convert branch diameter (in)
			glassout$jcore = glassout$jcore/100 # Convert juvenile core diameter (in)
			glassout$idib = glassout$idib/100 # Convert inside bark diameter (in)
			glassout = glassout[glassout$brht!=0,] # Constrain to trees with information by way of branch height
			glassout = glassout[glassout$mgexp!=0,] # Constrain to cut trees
			glassout = glassout[order(glassout$period,glassout$subperiod,glassout$tree,glassout$brht),]	# Order by the regular index values 
			
			## Calculate volume for each tree
			TERROR=rep(0,2000*4);TWARNING=rep(0,2000*4);VERROR=rep(0,5);VWARNING=rep(0,5);CFVOL=rep(0,2000);BFVOL=rep(0,2000);
			
			## Supply arguments to each volume subroutine vector
			CFTD = unit$cftd # Top diameter inside bark for cubic foot volume
			CFSH = unit$cfsh # Stump height for cubic foot volume 
			LOGLL = unit$logll # Log length for Scribner volume
			LOGML = unit$logml # Minimum log length for Scribner volume
			LOGTD = unit$logtd # Top diameter inside bark for Scriber volume
			LOGSH = unit$logsh # Stump height for Scribner volume
			LOGTA = unit$logta # Trim allowance for Scribner volume
			
			## Estimate volume for the sampled trees
			dyn.load(paste(dlls,"CIPSVOL.dll",sep="/"),type="Fortran") # Load the Organon volume subroutine
			
			## Depending on tree list size, simply process or split into equal intervals and process
			if(nrow(executed)<2000){
				## Calculate volume estimates for each tree			
				NTREES=nrow(executed) # Number of trees to process 
				SPECIES[1:NTREES] <- executed$species # Tree species
				DBH[1:NTREES] <- executed$dbh # Diameter at breast height (in)
				HT[1:NTREES] <- executed$tht # Total tree height (ft)
				CR[1:NTREES] <- executed$cr # Crown ratio 
				volume = .Fortran("volcal",as.integer(VERSION),as.integer(NTREES),as.integer(SPECIES),as.single(CFTD),as.single(CFSH),as.single(LOGLL),as.single(LOGML),as.single(LOGTD), 
						as.single(LOGSH),as.single(LOGTA),as.single(DBH),as.single(HT),as.single(CR),as.integer(VERROR),as.integer(TERROR),as.integer(VWARNING), 
						as.integer(TWARNING),as.integer(IERROR),as.single(CFVOL),as.single(BFVOL))
				executed$cfv = volume[[19]][1:NTREES] # Input the cubic foot volume for each tree
				executed$bfv = volume[[20]][1:NTREES] # Input the board foot volume for each tree
				
			} else {
				int = findInterval(1:nrow(executed),seq(2001,2001*CYCLG,by=2000)) # Find spliting intervals [every 2000]
				executed=split(executed,int) # Split up the tree list by the found interval(s)
				executed = lapply(executed,function(df){
							## Calculate volume estimates for each tree			
							NTREES=nrow(df) # Number of trees to process 
							SPECIES[1:NTREES] <- df$species # Tree species
							DBH[1:NTREES] <- df$dbh # Diameter at breast height (in)
							HT[1:NTREES] <- df$tht # Total tree height (ft)
							CR[1:NTREES] <- df$cr # Crown ratio 
							volume = .Fortran("volcal",as.integer(VERSION),as.integer(NTREES),as.integer(SPECIES),as.single(CFTD),as.single(CFSH),as.single(LOGLL),as.single(LOGML),as.single(LOGTD), 
									as.single(LOGSH),as.single(LOGTA),as.single(DBH),as.single(HT),as.single(CR),as.integer(VERROR),as.integer(TERROR),as.integer(VWARNING), 
									as.integer(TWARNING),as.integer(IERROR),as.single(CFVOL),as.single(BFVOL))
							df$cfv = volume[[19]][1:NTREES] # Input the cubic foot volume for each tree
							df$bfv = volume[[20]][1:NTREES] # Input the board foot volume for each tree
							df # Return the processed (cut section) data frame
							
						}
				)
				executed = unsplit(executed,int) # Return to native format
			}
			dyn.unload(paste(dlls,"CIPSVOL.dll",sep="/")) # Unload the volume calculation DLL
			
			executed = cbind(data.frame(variant=1,unit=unit$unit),executed) # Variant and unit identifiers				
		}
		
		## Finalize the flag output; especially important to deal with any fatal errors! 
		flags$stand = cbind(unit=unit$unit,flags$stand) # Associate unit index
		flags$stand <- flags$stand[flags$stand$exists==1,] # Only select errors which were found
		flags$stand = subset( flags$stand,select=-c(exists) ) # Drop unnecessary columns
		flags$tree = cbind(data.frame(unit=sample$unit,sample=sample$sample,tree=sample$tree),flags$tree) # Associate the number so user can track error down
		flags$tree = flags$tree[apply(subset(flags$tree,select=-c(unit,sample,tree)), 1, function(row) any(row !=0 )),] # Retain only rows with a flagged value	
		
		## Populate the output list, with the existing output; no output results in a zero value instead of data frame
		cipsanon.out=list()  
		if(fatal==0){cipsanon.out[[1]]=executed}else{cipsanon.out[[1]]=NULL} 
		if(fatal==0){cipsanon.out[[2]]=glassout}else{cipsanon.out[[2]]=NULL}
		cipsanon.out[[3]]=flags$stand
		cipsanon.out[[4]]=flags$tree
		
		return(cipsanon.out) # Return the list
	}
	
	## Function to run the vanilla variant of Organon (Organon) for a single sample
	.organon <- function( sample, unit, activity ) {
		
		n = nrow(sample) # The number of trees included in the 'big sample'; a combination of many samples
		
		## Errors can occur when running Organon; load the possible error table
		flags = read.csv(paste(warn,"flags.csv",sep="/")) # Read the table into R
		flags = split(flags,flags$level) # Split up by stand and tree level errors
		flags$stand$exists = 0 # Assume that stand level errors do not exist 
		## Reformat tree level errors given sample size dimensions assuming no errors exist
		flags$tree = as.data.frame(matrix(0,nrow=n,ncol=17,dimnames=list(1:n,unique(flags$tree$flag))))
		
		## Format Organon vectors for use in the Fortran subroutine 
		SPECIES=rep(0,2000);USER=rep(0,2000);DBH=rep(0,2000);HT=rep(0,2000);CR=rep(0,2000);EXPAN=rep(0,2000);RADGRO=rep(0,2000);IERROR=0;
		RVARS=rep(0,30);SERROR=rep(0,13);TERROR=rep(0,2000*6);SWARNING=rep(0,8);TWARNING=rep(0,2000);GROWTH=rep(0,2000);ACALIB=rep(0,3*18)
		
		## Supply arguments to each prepare subroutine vector
		VERSION = unit$version # Version of Organon to run
		NPTS = length(unique(sample$sample)) # Number of sample points fixed at one
		NTREES = n # Number of trees in the sample
		STAGE = unit$stage # Stand age
		BHAGE = unit$bhage # Breast height age
		SPECIES[1:n] = sample$species # Species for each tree
		USER[1:n] = sample$user # Code for user thinning 
		IEVEN = unit$iseven # If the stand is even aged or not
		DBH[1:n] = sample$dbh # Diameter at breast height for each tree (in)
		HT[1:n] = sample$tht # Total height for each tree (ft)
		CR[1:n] = sample$cr # Crown ratio for each tree 
		EXPAN[1:n] = sample$expan # Expansion factor for each tree (tpa)
		RADGRO[1:n] = sample$radgro # User supplied estimates of radial growth (in)
		RVARS[1:6] = c(unit$dfsi,unit$otsi,unit$dfsdi,unit$wgsdi,unit$phsdi,0) # Set of six indicator variables
		IRAD = if(any(RADGRO > 0)) {1} else {0} # Indicator if radial growth measurements were entered
		
		dyn.load(paste(dlls,"ORGEDIT.dll",sep="/"),type="Fortran") # Prepare subroutine
		prepared = .Fortran("prepare",as.integer(VERSION),as.integer(NPTS),as.integer(NTREES),as.integer(STAGE),
				as.integer(BHAGE),as.integer(SPECIES),as.integer(USER),as.integer(IEVEN),as.single(DBH),
				as.single(HT), as.single(CR),as.single(EXPAN),as.single(RADGRO),as.single(RVARS),as.integer(SERROR), 
				as.integer(TERROR), as.integer(SWARNING),as.integer(TWARNING),as.integer(IERROR),as.integer(IRAD),
				as.single(GROWTH),as.single(ACALIB)) 
		dyn.unload(paste(dlls,"ORGEDIT.dll",sep="/")) # Unload the prepare subroutine
		
		fatal = prepared[[19]] # Check if a fatal error occured while processing the sample
		if(fatal==1) {winDialog("ok",paste("Fatal Error Occured While Preparing Unit",unit$unit))} # Report if a fatal error occured during the prepare routine
		
		## Extract stand and tree level warnings and errors from the prepare subroutine
		flags$stand$exists[flags$stand$routine=="prepare"] <- c(prepared[[15]],prepared[[17]])
		for(z in 1:5) {flags$tree[,z] <- prepared[[16]][(1+(z-1)*2000):(2000*(z-1)+n)]}
		flags$tree[,7] <- prepared[[18]][1:n]
		
		## Provided no fatal errors, procedure to growing the stand
		if(fatal==0) {
			
			## Format Organon vectors for use in the Fortran subroutine
			TREENO=rep(0,2000);PTNO=rep(0,2000);SPECIES=rep(0,2000);USER=rep(0,2000);INDS=rep(0,30);DBH1=rep(0,2000);HT1=rep(0,2000);
			CR1=rep(0,2000);SCR1=rep(0,2000);EXPAN1=rep(0,2000);MGEXP=rep(0,2000);RVARS=rep(0,30);ACALIB=rep(0,3*18);PN=rep(0,5);
			YSF=rep(0,5);BART=rep(0,5);YST=rep(0,5);NPR=rep(0,2000);PRAGE=rep(0,2000*3);PRLH=rep(0,2000*3);PRDBH=rep(0,2000*3);
			PRHT=rep(0,2000*3);PRCR=rep(0,2000*3);PREXP=rep(0,2000*3);BRCNT=rep(0,2000*3);BRHT=rep(0,2000*40);BRDIA=rep(0,2000*40);
			JCORE=rep(0,2000*40);SERROR=rep(0,35);TERROR=rep(0,2000*6);SWARNING=rep(0,9);TWARNING=rep(0,2000);DGRO=rep(0,2000);
			HGRO=rep(0,2000);CRCHNG=rep(0,2000);SCRCHNG=rep(0,2000);MORTEXP=rep(0,2000);NTREES2=0;DBH2=rep(0,2000);HT2=rep(0,2000);
			CR2=rep(0,2000);SCR2=rep(0,2000);EXPAN2=rep(0,2000);STOR=rep(0,30);CYCLG=0;BABT=0;IDIB=rep(0,2000*40);
			
			## Supply arguments to each grow subroutine vector
			VERSION = prepared[[1]] # Version of Organon to run
			NPTS = prepared[[2]] # Number of sample points fixed at one
			NTREES1 = prepared[[3]] # Number of trees prior to growing
			STAGE = prepared[[4]] # Stand age
			BHAGE = prepared[[5]] # Breast height age 
			if(all(sample$tree==0)) {TREENO[1:n]=1:n} else {TREENO[1:n]=sample$tree} ## If all tree number set to zero, create tree numbers for the 'big sample'		
			PTNO[1:n] = sample$sample # Sample number corresponding to each tree
			SPECIES = prepared[[6]] # Species for each tree
			USER = prepared[[7]] # Code for user thinning
			# Set of 15 indicator variables
			INDS[1:15] = c(unit$dhcal,unit$ccal,unit$dgrocal,unit$iseven,unit$triple,0,unit$partcut,unit$pastfert,unit$maxsdi,unit$woodqual,0,0,0,unit$genes,unit$snc)
			DBH1 = prepared[[9]] # Diameter breast height for each tree (in), prior to growing   
			HT1 = prepared[[10]] # Total tree height for each tree (ft), prior to growing
			CR1 = prepared[[11]] # Crown ratio for each tree, prior to growing
			EXPAN1 = prepared[[12]] # Expansion factor for each tree (tpa), prior to growing
			RVARS[1:9] = c(unit$dfsi,unit$otsi,unit$dfsdi,unit$wgsdi,unit$phsdi,unit$gdval,unit$ghval,unit$dfret,0) # Set of 9 additional indicator values
			ACALIB = prepared[[22]] # Prepared calibration values
			
			# Supply arguments to each wood quality subroutine vector
			IJCALC = unit$core # Definition of the juvenile wood core
			IFINAL = 0 # Indicator for a final felling
			ACTION = 1 # Specify an initial fill in of wood quality values
			NINGRO = 0 # Number of ingrowth trees
			NTREES = NTREES1 # Number of trees
			NWQT = 0 # Internal value of wood quality DLL
			SITE_1 = unit$dfsi # Douglas-fir site index
			SITE_2 = unit$otsi # Other site index
			PDEN = 0 # Starting number of Red Alder trees per acre
			SCR = SCR1 # Shadow crown ratio
			
			## If user specified quality values, update vectors for wood quality
			if(unit$woodqual==1) {
				
				# If the wood quality routine is in use, it is necessary to update input with imputated values 
				CR = CR1 # Update crown ratio (in case of imputation)
				HT = HT1 # Update tree height (in case of imputation)
				
				dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
				glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
						as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
						as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
				dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the Organon wood quality subroutine
				
				BRCNT = glasslog[[26]] # Branch count
				BRDIA = glasslog[[27]] # Branch diameter
				BRHT = glasslog[[28]] # Branch height 
				JCORE = glasslog[[29]] # Juvenile core diameter
				IDIB = glasslog[[30]] # Inside bark diameter 
				NWQT = glasslog[[10]] # Internal variable
			}
			
			## Prior to growing, create a blank matrix to store output from growing the stand
			executed = matrix(ncol=14,nrow=(2000*unit$groyrs/5)) # Set matrix dimensions equal to the maximum size possible under the projection length
			## Specify the column names, and built a blank, 'big' tree list
			colnames(executed) <- c("sample","period","subperiod","stage","user","tree","species","dbh","tht","cr","expan","mgexp","cfv","bfv") 
			executed = as.data.frame(executed) # Convert to a data frame to facilitate referencing by column name
			## Insert the initial sample values prior to growing; Note that tree number is the user submitted identifier 
			executed[1:n,1:12] <- cbind(PTNO[1:n],rep(0,n),rep(0,n),rep(STAGE,n),USER[1:n],TREENO[1:n],SPECIES[1:n],DBH1[1:n],HT1[1:n],CR1[1:n],EXPAN1[1:n]/NPTS,MGEXP[1:n]/NPTS)
			rtab = n + 1 # Row tab to index the placement of new values during the growth procedure
						
			## Grow the sample to user-specified rotation age
			for(k in 1:(unit$groyrs/5)) {
				
				## Calculate values used to evaluate if a treatment should be applied each iteration
				triggers = data.frame(year=STAGE,tpa=sum(EXPAN1/NPTS),bap=sum(pi*(DBH1/2)^2/144*(EXPAN1/NPTS)))
				triggers$qmd = with(triggers,sqrt(bap/(0.005454*tpa)))
				triggers$sdi = with(triggers,tpa*(qmd/10)^1.605)
				triggers$rel = triggers$bap/(triggers$qmd^0.5) 
				
				## Evaluate if a treatment should be applied, and update if conditions are met
				inform = .treatment(executed,activity,triggers) 	
				
				## Inform the program about thinning treatments if they were made
				if(inform[[1]][1]==1) {	
					
					## Update elements associated with the execute subroutine
					executed = rbind(executed,inform[[2]]) # Attach new subperiod to the very end of the treelist 
					BABT = triggers$bap # Basal area before thinning (ft2/acre)
					EXPAN1[1:NTREES1] = inform[[2]]$expan*NPTS # Unadjusted expansion factor for each tree (tpa)
					MGEXP[1:NTREES1] = inform[[2]]$mgexp*NPTS # Unadjusted expansion factor for cut trees (tpa)									
					BART[which(BART>0)+1] <- BART[which(BART>0)] # Slide all non-zero basal area removed values up a position
					BART[1] = BABT-sum(pi*(inform[[2]]$dbh/2)^2/144*inform[[2]]$expan) # Calculate basal area removed by thinning (ft2/ac)
					YST[which(YST>0)+1] <- YST[which(YST>0)] # Slide YST values up a position
					YST[1] = CYCLG*5 # Set years since thinning (* in five year increments)
					INDS[13][any(SPECIES[MGEXP>0] %in% c(15,17,81,117,122,202,231,242,263))] <- 1 # Indicate if major conifers were cut
					INDS[11] <- 1 # Indicator that an overstory tree was harvested
					INDS[7] <- 1 # Indicator for partial cutting of the sample
					
					## Finalize wood quality for thinned trees
					ACTION = 4 # Set action to finalize thinned tree value
					NTREES = NTREES1 # Update the vectors for wood quality
					DBH = DBH1 # Diameter at breast height for each tree (in)
					HT = HT1 # Total height for each tree (ft)
					CR = CR1 # Crown ratio for each tree 
					SCR = SCR1 # Shadow crown ratio for each tree (tpa)
					EXPAN = EXPAN1 # Expansion factor for each tree (tpa)
					
					## If user specified quality values, update vectors for wood quality
					if(unit$woodqual==1) {
						dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
						## Calculate wood quality attributes for thinned trees
						glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
								as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
								as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
						dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the Organon wood quality subroutine
						
						## Update relevant wood quality vectors
						BRCNT = glasslog[[26]] # Branch count
						BRDIA = glasslog[[27]] # Branch diameter
						BRHT = glasslog[[28]] # Branch height 
						JCORE = glasslog[[29]] # Juvenile core diameter
						IDIB = glasslog[[30]] # Inside bark diameter 
						NWQT = glasslog[[10]] # Internal variable
					}
					
					## Capture wood quality values and associate with any thinned tree values if they exist				
					glassin = data.frame(sample=rep(PTNO,40),period=CYCLG,subperiod=1,stage=STAGE,tree=rep(TREENO,40),mgexp=rep(MGEXP/NPTS,40),brht=BRHT,brdia=BRDIA,jcore=JCORE,idib=IDIB)
					if(exists("glassout")) {glassout=rbind(glassout,glassin)} else {glassout=glassin} # Bind in, or name the wood quality output				
				}
				
				## If fertilization treatment was made, supply arguments to inform the execute subroutine 
				if(inform[[1]][2]==1) {	
					PN[which(PN>0)+1] <- PN[which(PN>0)] # Slide all non-zero values for pounds fertilized applied
					PN[1] = inform[[3]] # Update with pounds of fertilizer applied 
					YSF[which(YSF>0)+1] <- YSF[which(YSF>0)] # Slide all non-zero values for years since fertilization
					YSF[1] = CYCLG*5 # Update the years since fertilization (* in five year increments)
					INDS[8] <- 1 # Indicate the stand has been fertilized
				}
				
				## Call the execute subroutine in ORGRUN.dll to grow the sample
				dyn.load(paste(dlls,"ORGRUN.dll",sep="/"),type="Fortran") # Load the execute subroutine 
				grow = .Fortran("execute",as.integer(CYCLG), as.integer(VERSION),as.integer(NPTS), as.integer(NTREES1), as.integer(STAGE), as.integer(BHAGE), 
						as.integer(TREENO), as.integer(PTNO), as.integer(SPECIES), as.integer(USER),as.integer(INDS), as.single(DBH1), as.single(HT1), as.single(CR1), 
						as.single(SCR1), as.single(EXPAN1), as.single(MGEXP),as.single(RVARS), as.single(ACALIB), as.single(PN), as.single(YSF),as.single(BABT), 
						as.single(BART), as.single(YST), as.integer(NPR),as.integer(PRAGE), as.single(PRLH), as.single(PRDBH), as.single(PRHT), as.single(PRCR), 
						as.single(PREXP), as.integer(BRCNT), as.integer(BRHT),as.integer(BRDIA), as.integer(JCORE), as.integer(SERROR), as.integer(TERROR), 
						as.integer(SWARNING), as.integer(TWARNING), as.integer(IERROR),as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG), 
						as.single(MORTEXP), as.integer(NTREES2), as.single(DBH2), as.single(HT2),as.single(CR2), as.single(SCR2), as.single(EXPAN2), as.single(STOR)) 
				dyn.unload(paste(dlls,"ORGRUN.dll",sep="/")) # Unload the execute subroutine 
				
				## Update the values for the grown sample
				TREENO = grow[[7]] # Tree numbers
				PTNO = grow[[8]] # Sample number
				SPECIES = grow[[9]] # Tree species
				USER = grow[[10]] # Update the user code
				NTREES1 = grow[[46]] # Number of trees
				STAGE = grow[[5]] # Stand age
				BHAGE = grow[[6]] # Breast height age
				DBH1 = grow[[47]] # Diameter at breast height for each tree (in)
				HT1 = grow[[48]] # Total height for each tree (ft)
				CR1 = grow[[49]] # Crown ratio for each tree
				SCR1 = grow[[50]] # Shadow crown ratio for each tree
				EXPAN1 = grow[[51]] # Expansion factor for each tree (tpa)
				CRCHNG = grow[[43]] # Change in crown ratio for each tree
				SCRCHNG = grow[[44]] # Change in shadow crown ratio for each tree
				STOR = STOR[CYCLG==0] <- grow[[52]] # Obtain only on the first call  
				DGRO = grow[[41]] # Diameter growth rate (in)
				HGRO = grow[[42]] # Height growth rate (ft)
				
				fatal = grow[[40]] # Indicator for a fatal error occur while growing the stand
				if(fatal==1) {break()} # If a fatal error occured, break loop and do not accumulate any values 
				
				## Insert the updated tree values into the executed value table; As before, the user submitted tree number is assigned			
				executed[rtab:(rtab+NTREES1-1),1:12] <- cbind(PTNO[1:NTREES1],rep(k,NTREES1),rep(0,NTREES1),rep(STAGE,NTREES1),USER[1:NTREES1],TREENO[1:NTREES1],SPECIES[1:NTREES1],
						DBH1[1:NTREES1],HT1[1:NTREES1],	CR1[1:NTREES1],EXPAN1[1:NTREES1]/NPTS,MGEXP[1:NTREES1]/NPTS)
				
				## Update wood quality attributes from the grown tree information
				ACTION = 3 # By default, add whorls each cycle
				NTREES = NTREES1 # Number of trees
				DBH = DBH1 # Diameter at breast height for each tree (in)
				HT = HT1 # Total height for each tree (ft)
				CR = CR1 # Crown ratio for each tree 
				SCR = SCR1 # Shadow crown ratio for each tree
				EXPAN = EXPAN1 # Expansion factor for each tree (tpa)
				BRCNT = grow[[32]] # Branch count
				BRHT = grow[[33]] # Branch height
				BRDIA = grow[[34]] # Branch diameter
				JCORE =  grow[[35]] # Juvenile core 
				
				## If user specified quality values, update vectors for wood quality
				if(unit$woodqual==1 & inform[[1]][1]!=1 ) {
					dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type="Fortran") # Load the Organon wood quality subroutine
					## Call the wood quality subroutine in ORGWQ.dll to estimate wood quality
					glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
							as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
							as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
					dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the Organon wood quality subroutine
					
					BRCNT = glasslog[[26]] # Branch count
					BRDIA = glasslog[[27]] # Branch diameter
					BRHT = glasslog[[28]] # Branch height 
					JCORE = glasslog[[29]] # Juvenile core diameter
					IDIB = glasslog[[30]] # Inside bark diameter 
					NWQT = glasslog[[10]] # Internal variable
				}
				
				## Update iterative indexing values and restore some subroutine vectors
				rtab = rtab + NTREES1 # Update the row tab to index the placement of new values during the growth procedure
				MGEXP = rep(0,2000) # Expansion factor for cut trees
				INDS[11] = 0 # Reset indicator for cutting overstory trees
				INDS[13] = 0 # Reset indicator for cutting major conifer trees
				CYCLG = k # Update the value of cycles grown so far
			}	
			
			if(fatal==1) {winDialog("ok",paste("Fatal Error Occured While Growing Unit",unit$unit))} # Report if a fatal error occured during the growth routine
			
			executed = executed[!is.na(executed$period), ] ## Recalling that maximum possible matrix space was added to place grown tree values, reduce to populated rows
			
			## Extract stand and tree level warnings and errors from the execute subroutine
			flags$stand$exists[flags$stand$routine=="execute"] <- c( grow[[36]],grow[[38]] ) 
			for( z in 1:6 ) { flags$tree[,z] <- grow[[37]][(1+(z-1)*2000):(2000*(z-1)+n)] }
			flags$tree[,7] <- grow[[39]][1:n]
		}
		
		## If a fatal error did not occur during growth, final-fell the forest 
		if(fatal==0) {
			## Input final-felling information as a subperiod (1)
			rtab = nrow(executed) + 1 # Update the row index value
			executed = rbind(executed,executed[executed$period==CYCLG & executed$subperiod==0,]) # Create space for the final-felling period
			executed[rtab:nrow(executed),]$subperiod <- 1 # Specify the subperiod as a final felling (1); Thinning not possible at final cycle
			executed[executed$period==CYCLG & executed$subperiod==1,]$mgexp <- executed[executed$period==CYCLG & executed$subperiod==1,]$expan # Update cut tree expansion factor
			executed[executed$period==CYCLG & executed$subperiod==1,]$expan <- 0 # No trees have an expansion factor after harvest
			
			## Calculate finalized wood quality values
			ACTION = 5 # Finalize wood quality values
			IFINAL = 1 # Indicate a final harvest
			EXPAN = rep(0,2000) # Expansion factor is set to zero
			MGEXP[1:NTREES1] = executed[rtab:nrow(executed),]$mgexp # Update the thinned tree expansion factor (tpa)
			
			## If user specified quality values, update vectors for wood quality
			if(unit$woodqual==1) {
				dyn.load(paste(dlls,"ORGWQ.dll",sep="/"),type=Fortran) # Load the wood quality DLL
				glasslog = .Fortran("woodqual",as.integer(IJCALC),as.integer(IEVEN),as.integer(IFINAL),as.integer(ACTION),as.integer(BHAGE),as.integer(STAGE),as.integer(NINGRO),as.integer(NPTS),as.integer(NTREES),as.integer(NWQT), 
						as.integer(VERSION),as.integer(SPECIES),as.single(SITE_1),as.single(SITE_2),as.single(PDEN),as.single(DBH),as.single(HT),as.single(CR),as.single(SCR),as.single(EXPAN),as.single(MGEXP), 
						as.single(DGRO), as.single(HGRO), as.single(CRCHNG), as.single(SCRCHNG),as.integer(BRCNT), as.integer(BRDIA), as.integer(BRHT), as.integer(JCORE), as.integer(IDIB) )
				dyn.unload(paste(dlls,"ORGWQ.dll",sep="/")) # Unload the wood quality subroutine
				
				BRCNT = glasslog[[26]] # Branch count
				BRDIA = glasslog[[27]] # Branch diameter
				BRHT = glasslog[[28]] # Branch height 
				JCORE = glasslog[[29]] # Juvenile core diameter
				IDIB = glasslog[[30]] # Inside bark diameter 
				NWQT = glasslog[[10]] # Internal variable
			}
			
			## Capture final-felling wood quality values and associate with any thinned tree values if they exist	
			glassin = data.frame(sample=rep(PTNO,40),period=CYCLG,subperiod=1,stage=STAGE,tree=rep(TREENO,40),mgexp=rep(MGEXP,40),brht=BRHT,brdia=BRDIA,jcore=JCORE,idib=IDIB)
			if(exists("glassout")) {glassout=rbind(glassout,glassin)} else {glassout=glassin} # Bind in, or name the wood quality output
			
			## Prepare the wood quality information for output		
			glassout = cbind(unit=unit$unit,glassout) # Assign the unit identifier 
			glassout$brht = glassout$brht/10 # Convert branch height (ft)
			glassout$brdia = glassout$brdia/100 # Convert branch diameter (in)
			glassout$jcore = glassout$jcore/100 # Convert juvenile core diameter (in)
			glassout$idib = glassout$idib/100 # Convert inside bark diameter (in)
			glassout = glassout[glassout$brht!=0,] # Constrain to trees with information by way of branch height
			glassout = glassout[glassout$mgexp!=0,] # Constrain to cut trees
			glassout = glassout[order(glassout$period,glassout$subperiod,glassout$tree,glassout$brht),]	# Order by the regular index values 
			
			## Calculate volume for each tree
			TERROR=rep(0,2000*4);TWARNING=rep(0,2000*4);VERROR=rep(0,5);VWARNING=rep(0,5);CFVOL=rep(0,2000);BFVOL=rep(0,2000)
			
			## Supply arguments to each volume subroutine vector
			CFTD = unit$cftd # Top diameter inside bark for cubic foot volume
			CFSH = unit$cfsh # Stump height for cubic foot volume 
			LOGLL = unit$logll # Log length for Scribner volume
			LOGML = unit$logml # Minimum log length for Scribner volume
			LOGTD = unit$logtd # Top diameter inside bark for Scriber volume
			LOGSH = unit$logsh # Stump height for Scribner volume
			LOGTA = unit$logta # Trim allowance for Scribner volume
			VERSION[VERSION>1] <- 2 # Modify version to make CIPSVOL compatible with Organon
			
			## Estimate volume for the sampled trees
			dyn.load(paste(dlls,"CIPSVOL.dll",sep="/"),type=Fortran) # Load the Organon volume subroutine
			
			## Depending on tree list size, simply process or split into equal intervals and process
			if(nrow(executed)<2000){
				## Calculate volume estimates for each tree			
				NTREES=nrow(executed) # Number of trees to process 
				SPECIES[1:NTREES] <- executed$species # Tree species
				DBH[1:NTREES] <- executed$dbh # Diameter at breast height (in)
				HT[1:NTREES] <- executed$tht # Total tree height (ft)
				CR[1:NTREES] <- executed$cr # Crown ratio 
				volume = .Fortran("volcal",as.integer(VERSION),as.integer(NTREES),as.integer(SPECIES),as.single(CFTD),as.single(CFSH),as.single(LOGLL),as.single(LOGML),as.single(LOGTD), 
						as.single(LOGSH),as.single(LOGTA),as.single(DBH),as.single(HT),as.single(CR),as.integer(VERROR),as.integer(TERROR),as.integer(VWARNING), 
						as.integer(TWARNING),as.integer(IERROR),as.single(CFVOL),as.single(BFVOL))
				executed$cfv = volume[[19]][1:NTREES] # Input the cubic foot volume for each tree
				executed$bfv = volume[[20]][1:NTREES] # Input the board foot volume for each tree
				
			} else {
				int = findInterval(1:nrow(executed),seq(2001,2001*CYCLG,by=2000)) # Find spliting intervals [every 2000]
				executed=split(executed,int) # Split up the tree list by the found interval(s)
				executed = lapply(executed,function(df){
							## Calculate volume estimates for each tree			
							NTREES=nrow(df) # Number of trees to process 
							SPECIES[1:NTREES] <- df$species # Tree species
							DBH[1:NTREES] <- df$dbh # Diameter at breast height (in)
							HT[1:NTREES] <- df$tht # Total tree height (ft)
							CR[1:NTREES] <- df$cr # Crown ratio 
							volume = .Fortran("volcal",as.integer(VERSION),as.integer(NTREES),as.integer(SPECIES),as.single(CFTD),as.single(CFSH),as.single(LOGLL),as.single(LOGML),as.single(LOGTD), 
									as.single(LOGSH),as.single(LOGTA),as.single(DBH),as.single(HT),as.single(CR),as.integer(VERROR),as.integer(TERROR),as.integer(VWARNING), 
									as.integer(TWARNING),as.integer(IERROR),as.single(CFVOL),as.single(BFVOL))
							df$cfv = volume[[19]][1:NTREES] # Input the cubic foot volume for each tree
							df$bfv = volume[[20]][1:NTREES] # Input the board foot volume for each tree
							df # Return the processed (cut section) data frame
							
						}
				)
				executed = unsplit(executed,int) # Return to native format
			}
			dyn.unload(paste(dlls,"CIPSVOL.dll",sep="/")) # Unload the volume calculation DLL
			
			executed = cbind(data.frame(variant=1,unit=unit$unit),executed) # Variant and unit identifiers		
		}
		
		## Finalize the flag output; especially important to deal with any fatal errors! 
		flags$stand = cbind(unit=unit$unit,flags$stand) # Associate unit index
		flags$stand <- flags$stand[flags$stand$exists==1,] # Only select errors which were found
		flags$stand = subset( flags$stand,select=-c(exists) ) # Drop unnecessary columns
		flags$tree = cbind(data.frame(unit=sample$unit,sample=sample$sample,tree=sample$tree),flags$tree) # Associate the number so user can track error down
		flags$tree = flags$tree[apply(subset(flags$tree,select=-c(unit,sample,tree)), 1, function(row) any(row !=0 )),] # Retain only rows with a flagged value	
		
		## Populate the output list, with the existing output; no output results in a zero value instead of data frame
		organon.out=list()  
		if(fatal==0){organon.out[[1]]=executed}else{organon.out[[1]]=NULL} 
		if(fatal==0){organon.out[[2]]=glassout}else{organon.out[[2]]=NULL}
		organon.out[[3]]=flags$stand
		organon.out[[4]]=flags$tree
		
		rm(list=ls()[!(ls() %in% "organon.out")]); gc()
		return(organon.out) # Return the list
	}
	
	## Grow each unit in Organon, or Cipsanon, and iteratively tick up progress bar
	cipsr.out = lapply(units$unit,function(x){
				i = match(x,units$unit) # Note the progress in the iteration
				t <- i # Update iterator for progress and bar
				setWinProgressBar(pbar,t,title=paste("CIPS-R Progress: ",round(t/T*100,0),"%",sep="")) 
				## Route to the correct CIPSR variant
				if(units$variant[units$unit==x]==1) {
					out = .organon(subset(samples,unit==x),unit=subset(units,unit==x),subset(activities,unit==x))
				} else {
					out = .cipsanon(subset(samples,unit==x),unit=subset(units,unit==x),subset(activities,unit==x))
				}
			}
	)
	
	## Isolate and combine components of the finalized CIPS-R output list
	treelist = list() # Tree list is comprised of individual tree measurements
	for(j in 1:length(units$unit)) { treelist[[j]] = cipsr.out[[j]][[1]]; cipsr.out[[j]][[1]] <- NULL } # Place into a list
	treelist[sapply(treelist, is.null)] <- NULL # Remove fatal treelist output
	## Reduce the expansion factors for each unit given the samples installed
	treelist = lapply(treelist, function(x) {
				subset(x,select=-c(sample)) # Remove the sample index
				x # Return updated list elements
			}
	)
	
	## Calculate sample levels summary attributes at the period and subperiod level 
	samplelist = lapply(treelist, function(periods) {
				subperiods = split(periods,paste(periods$period,periods$subperiod))	
				out = lapply(subperiods, function(x) {
							n = nrow(x) # The number of trees in the sample
							variant = x$variant[1] # CIPSR variant that was employed
							unit = x$unit[1] # Unit index value
							period = x$period[1] # Period of projection
							subperiod = x$subperiod[1] # Subperiod of projection
							stage = x$stage[1] # Stand age (years)
							bap = sum( pi*(x$dbh/2)^2/144*x$expan) # Basal area per acre (ft2/acre)
							tpa = sum(x$expan) # Number of trees per acre 
							qmd = sqrt(bap/(0.005454*tpa)) # Quadradic mean diameter (in)
							qmd[is.na(qmd)] <- 0 # NaN produced by dividing zero bap and tpa values; set zero
							bfv = sum(x$bfv*x$expan) # Board foot per acre (scribner) 
							cfv = sum(x$cfv*x$expan) # Cubic foot volume per acre
							sdi = tpa*(qmd/10)^1.605 # Stand density index
							rel = bap/(qmd^0.5) # Relative density 			
							rel[is.na(rel)] <- 0 # Ensure, at final harvest, zero not null value
							data.frame(variant,unit,period,subperiod,stage,bap,tpa,qmd,sdi,rel,bfv,cfv)
						}
				)
				out = do.call("rbind",out) # Bind into a data frame
				out[order(out$unit,out$period,out$subperiod),] # Order for clarity
			}
	)
	samplelist=do.call("rbind",samplelist) # Bind into a data frame
	
	## Finalize tree list output
	treelist = do.call("rbind",treelist) # Bind the list into a data frame
	treelist = treelist[order(treelist$unit,treelist$period,treelist$subperiod),] # Order for clarity
	
	## Finalize wood quality output
	woodquality = list() # The wood quality list is comprised of individual tree and branch level measurements 
	for(j in 1:length(units$unit)) { woodquality[[j]] = cipsr.out[[j]][[1]]; cipsr.out[[j]][[1]] <- NULL  } # Place into a list
	woodquality[sapply(woodquality, is.null)] <- NULL # Remove fatal treelist output; or where output was not specified
	woodquality = do.call("rbind",woodquality) # Bind the list into a data frame	
	
	## Finalize stand and tree level flag output	
	standflags = list() # Stand level flags list comprised of unit specific messages
	for(j in 1:length(units$unit)) { standflags[[j]] = cipsr.out[[j]][[1]]; cipsr.out[[j]][[1]] <- NULL } # Place into a list
	standflags = do.call("rbind",standflags) # Bind the list into a data frame
	treeflags = list() # Tree level flags list comprised of unit specific messages
	for(j in 1:length(units$unit)) { treeflags[[j]] = cipsr.out[[j]][[1]]; cipsr.out[[j]][[1]] <- NULL } # Place into a list
	treeflags = do.call("rbind",treeflags) # Bind the list into a data frame
	
	rm(cipsr.out); gc() # Remove object; garbage collection
	
	## Process user requested plots
	if(any(units$wantplot==1|units$wantplot==2)) {
		
		## A data frame of information used to create a series of plots 
		plotinf = data.frame(heads=c("Basal Area Per Acre","Trees per Acre","Quadradic Mean Diameter","Stand Density Index","Relative Density",
						"Scribner Volume","Cubic Foot Volume"),labs=c("Square Feet per Acre","Number of Trees per Acre","Inches","Reinke's Stand Density Index",
						"Percentage","Board feet per acre","Cubic feet per acre"),gets=c("bap","tpa","qmd","sdi","rel","bfv","cfv")) 
		
		## If any plots should be displayed on the screen apply the following procedure
		if(any(units$wantplot==1)){
			## Process plots to be printed on the screen
			drawdat = samplelist[samplelist$unit %in% units$unit[units$wantplot==1],] # Subset to data for 'screen printing'
			
			## Print a series of plots on the screen
			sapply(as.character(plotinf$gets),function(i){
						y = drawdat[[i]] # The response variable
						x = drawdat$stage # The stand age
						# Create a blank plot, and draw lines by the unit
						par(mar=c(5.1, 4.1, 4.1, 8.1)) # Create a plot with sufficient room for a legend
						plot(x,y,type="n",main=plotinf$heads[plotinf$gets==i],ylab=plotinf$labs[plotinf$gets==i],xlab="Stand Age (Years)") # Draw a blank plot  
						df = split(drawdat,drawdat$unit) # Split the data up by units to draw lines for each attribute 
						df = df[unlist(lapply(df, nrow)!=0)] # Constrain to unit elements in the list with values; those with wantplot equal one
						par(xpd=TRUE) # Allow for plotting in the margins
						## Limit the legend based on the number of lines (greater than eight, legend irrelevant)
						if(nrow(units[units$wantplot==1,])<8){
							legend("topright",inset=c(-0.25,0),names(df),lty=1:length(unique(drawdat$unit)),title="Units",cex=1.2) # Inset a legend 
						}
						# Split up the drawing data by unit
						lapply(df,function(z){
									dash = match(unique(z$unit),unique(drawdat$unit)) # Use position in unique units to get line and point type
									lines(z$stage,z[[i]],lty=dash)	# Draw attribute of interest by period of projection
								}
						)
						par(ask=TRUE) # Ask to advance through plots
					}
			)
			par(ask=FALSE) # Turn off 'asking' for plots to advance
		}
		
		## If any plots should be printed into a directory, apply the following procedure
		if(any(units$wantplot==2)){
			
			## Process plots to be printed into a directory
			dir.create(paste(home,"My Output",sep="/"),showWarnings=FALSE) # Create a directory for printing
			printdat = samplelist[samplelist$unit %in% units$unit[units$wantplot==2],] # Subset to data for 'directory printing'
			## Print a series of plots to the output directory
			sapply(as.character(plotinf$gets),function(i){
						y = printdat[[i]] # The response variable
						x = printdat$stage # The stand age
						# Create a blank plot, and draw lines by the unit
						bmp(paste(home,"/My Output/",i,".bmp",sep=""),width=7.25,height=5.25,units="in",res=150)
						par(mar=c(5.1, 4.1, 4.1, 8.1)) # Create a plot with sufficient room for a legend
						plot(x,y,type="n",main=plotinf$heads[plotinf$gets==i],ylab=plotinf$labs[plotinf$gets==i],xlab="Stand Age (Years)")  
						df = split(printdat,printdat$unit) # Split the data up by units to draw lines for each attribute 
						df = df[unlist(lapply(df, nrow)!=0)] # Constrain to unit elements in the list with values; those with wantplot equal two
						par(xpd=TRUE) # Allow for plotting in the margins
						## Limit the legend based on the number of lines (greater than eight, legend irrelevant)
						if(nrow(units[units$wantplot==2,])<8){
							legend("topright",inset=c(-0.25,0),names(df),lty=1:length(unique(printdat$unit)),title="Units",cex=1.2) # Inset a legend 
						}
						# Split up the drawing data by unit
						lapply(df,function(z){
									dash = match(unique(z$unit),unique(printdat$unit)) # Use position in unique units to get line and point type
									lines(z$stage,z[[i]],lty=dash)	# Draw attribute of interest by period of projection
								}
						)
						dev.off() # Close the device to 'print' the next in the output directory 
					}
			)
		}
		
		graphics.off() # Ensure that all graphical devices have been turned off	
	}	
	
	## Combine the final amalgamated data frames into a finalized list
	cipsr.out = list(treelist=treelist,samplelist=samplelist,woodquality=woodquality,standflags=standflags,treeflags=treeflags) # Combine output information into a list
	cipsr.out = lapply(cipsr.out, function(x) {
				row.names(x)<-NULL; # Remove row names generated during splitting
				x[sapply(x,is.numeric)] <- round(x[sapply(x,is.numeric)],2); # Round to two decimal places 
				return(x) # Return the finalized object
			}
	)
	
	cipsr.out[[1]] = subset(cipsr.out[[1]],select=-c(sample)) # Now a giant sample, thus, identifier is not relevant 
	
	## If output tables are requested by the user, process them
	if(any(units$wanttable==1)){
		# Subset spreadsheet output to just those tables requested by the user
		spreadout = lapply(cipsr.out,function(x){
					x[x$unit %in% units$unit[units$wanttable==1],]
				}
		)
		
		## If an output directory exists, write to it, else create one
		if(!file.exists(paste(home,"My Output",sep="/"))){dir.create(paste(home,"My Output",sep="/"),showWarnings=FALSE)}
		
		## Write the user-specified output to the output directory
		writeWorksheetToFile(paste(home,"My Output/cips_output.xls",sep="/"),data=spreadout,
				sheet=c("treelist","samplelist","woodquality","standflags","treeflags"))			
	}
	
	setWinProgressBar(pbar,T,title=paste("CIPS-R Progress: ",100,"%",sep="")) # Set progress to 100%
	close(pbar) # Close the progress bar 
	
	return(cipsr.out) # Return the CIPS R output list
}	
