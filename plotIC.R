plotIC_rcs_id = "$Id: plotIC.R,v 1.012 2016-06-06 17:15:00-07 fran PCM $";
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright (c) 2016 Francesca Rizzardi.  All Rights Reserved.
# This is unpublished, proprietary code.  Do not use without permission.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# plot the factor IC's and dynamic score coefficients.
# Most args are described below, but here are details on some:

# The factor IC's are smoothed and may also include score IC's.
# In the case where there is a single factor and score, the score IC's are
# omitted.  (They are identical to the factor IC's.)
# If the input simulation creates neutralized scores, both original ("score.I")
# and final ("score.N") may be plotted.

# If there are multiple plots per run (number of factors exceeds maxperplot=)
# then all scores are plotted on all pages.

# If the input simulation used dynamic scoring, the plot of "final" factor
# coefficients, unsmoothed, appears below the smoothed IC's.

# sv:	sv object, normally from a <run>-sv.Rdata file.
#	Or, a file (filename) that contains an sv object, as determined by
#	sv.load.chk().  Note (a): sv.load.chk require you to specify "./file"
#	for "file" in cwd; otherwise it looks relative to the runs dir.
#	Also, sv.load.ch adds the "-sv" and ".Rdata" name components if needed.

# m:	Optional matrix whose columns are sv$cd$ic$score components, possibly
#	from multiple sv's.  We assume that all the parent sv's have the same
#	$pre$T and $cd$ic$date components.  These components are taken from sv
#	If NULL, m is the IC's in sv$cd$ic.  See sf= for details.
#	ALERT: m must have column names that consist of the names of the factors
#	and score names of m's columns.

# sf:   The factors or scores for which to plot the IC's, specified in the
#	form of column names of m (if m is given) or factor and score names
#	otherwise.
#	If m is given, default is its columns.
#	If m=NULL, default is:
#	    - All factors used in the input simulation scoring step
#	    - The final score, "score.N", if the score was neutralized;
#	      otherwise, the final score, "score".
#	    - If the score was neutralized, the original score, "score.I" if
#	      scoreI=TRUE.
#	    - In the case of sm="none":
#		* sv$sc$par$scoreu is added and plotted like a score.
#		* sv$pre$factag is added.

# driver: Choices are "x" or "pdf".  If NULL, use "pdf" if file is given, "x"
#	if file is NULL.  plotIC does not (yet) use choose_driver(), because
#	the plot sizes still need to be tuned.

# file:	Output file name if driver is "pdf".  If NULL, filename is
#	sv$ob$obase-plot.pdf.  If overwrite is false (0) and if that file
#	exists, the filename is sv$ob$obase-plotN.pdf where N is the smallest
#	positive integer such that sv$ob$obase-plotN.pdf doesn't already exist.
#	If multiple plots are produced, each plot gets its own page in the PDF.

# colors: vector of colors for the IC's, in order of sf.
#	Default factor colors are created by color_grp() which is documented
#	in plotsub.R.
#	Colors can be entered as any type (string, hex, RGB, etc) allowed by R.
#	In all cases where input color values are in hex, those which are too
#	close to yellow are replaced by shades of brown.  See sub_brown() in
#	plotsub.R for details.
#	The color "yellow" is replaced by "brown" for visibility.
#	Scores are plotted as "black".
#	If there is more than one page of plots, colors start over at each
#	page.

# porder: plotting order of the IC's.
#	This option refers to the physical order in which the plot lines are
#	drawn and hence which one goes "on top" when lines are very close.
#	Default is "f": forward, same order as sf.
#	porder="b": backwards from the sf order.
#	Otherwise porder is a vector of 1 through (number scores) in any order.

# scorediff: If true, plot (score.N - score.I) as a shaded area.

# means: Smoothing means function applied to the IC's being plotted.
#	Allowable values are "roll", rolling means, or any value allowed by
#	geomwt() for its p= parameter, in which case geometric weights are
#	applied.  Default is sv$sc$par$dssp= if set, "tri" otherwise.

# p:	Mnemonic for "means".  If non-null, overrides means.

# nroll: Window size of the smoothing means specified by means=.
#	Number of months of data in the smoothed or rolling means.
#	Default is c(24,12,6,3)
#	If nroll=NULL then use sv$sc$par$dssn= if given, else nroll=24

# annual: Plot annual means on upper plot.  Default is FALSE.
#	annual=TRUE is currently broken for dynamic scores.

# yrange: Plotting range for the IC's.  Default is c(-M,M) where M = max(|m|)

# maxperplot: Number of factors per plot (one plot per page).  All scores are
#	always plotted on each page.  If colors="dflt" then the default is
#	all factors on a single page.  Otherwise, the default is maxperplot=6.
# 	Should not exceed the number of colors (see above).

# startup: number of initial months to not plot.  NULL means use
#	startup=sv$sc$par$dssn if given, else startup=24

# quiet: Suppress the "plotIC created ..." output message

# EXAMPLES

#	The default way to run is:
#	   > plotIC(sv)
#	If dynamic scores with smoothing, the default means= and nroll= are
#	dssp dssn to use the same smoothing for the IC's and smoothed factor
#	coefficients in the lower plot.  Otherwise, means='tri' and nroll=24.

#	A more complex example: for the same stock universe, factors, and time
#	period, we want to compare the IC's for a dynamic run (in sv file svd)
#	to the score IC of a fixed factor coefficient run (in sv file svf).
#	In this case, we use the m input where the columns of m consist of IC's
#	from both runs.

#	   > m1 = svd$cd$ic[,c(svd$sc$par$sfu,"score")]
#	   > scoref = svf$cd$ic$score
#	   > mx = cbind(m1,scoref)
#          > plotIC(svd, mx)

#	We took advantage of a feature of cbind() that names the scoref column
#	"scoref" so that plotIC() uses its special color and line types for
#	scores to plot it.

# OUTSTANDING SPECIAL CASES
# If sv$sc$par$sfu is NULL (sv comes from a run with sm="none") then no
# score* factors will be plotted like scores.  This might occur, for example,
# if m is given and contains scores from another run.

plotIC = function(
    sv,		   # sv object; or a file as explained above.
    m=NULL,	   # see above
    sf=NULL, 	   # names of the factors or scores of plotted IC's.
    static.score=NULL,
		   # if non-null, name of static score to add after all other
		   # factors and scores.
    scoreI=TRUE,   # include score.I if it exists and sf=NULL, m=NULL.
    titl1=NULL,    # Overall title.  If NULL, use sv$ob$obase
    driver=NULL,   # graphics driver.  See above.
    file=NULL,     # Output file for pdf.  See above for default.
    overwrite=0,   # Applies when file=NULL.  1 = overwrite pre-existing file.
			# 0 = find new file name.
    #pgsub=NULL,   # subtitle on first plot
    colors="dflt", # plotting colors
    porder="f",    # Plotting order of IC's
    fallback.indic=TRUE,
    		   # plot diamonds at months with fallback (dynscore)
    nametrans=FALSE, # translate factor/score names for the legend using
    		   # lookup into name_trans?
    scorediff=FALSE,	# plot score.N-score.I?
    means=NULL,    # Type of means of IC's to plot on upper plot.
    p=NULL,    	   # Same as means=; overrides it.
    nroll=c(24,12,6,3),    # see above.
    annual=FALSE,  # plot annual means on upper plot.
    yrange=NULL,   # Plotting range for the IC's
    fmax=NULL,     # maximum factor weight value to plot on factor coef plot.
    maxperplot=NA, # maximum number of factors (not scores) per plot.
		   # See above for details.
    startup=NULL,
    box=FALSE,	   # put a box around the entire plot
    yround=2.5,	   # round Y-axis (smoothed IC's) extrema to this value
    quiet=FALSE
    )
    {
    sv_file = NULL
    have_static.score = FALSE
    if (is.character(sv)) {
	sv_file = sv	# for possible use below
	sv = sv.load.chk(sv_file) # see note (a) above.
	if (is.character(sv)) return(NULL)  # error return from sv.load.chk()
    	}
    else if (!("sv" %in% class(sv))) {
	cat("plotIC: argument sv does not have class sv\n");
	return;
	}

    if (is.null(driver))
	driver = if (is.null(file)) "x" else "pdf"

    if (!is.null(sv_file)) {
	# If sv was given as a filename, let that name trump the internally
	# recorded obase, in case the original sv file was renamed.
	obase = gsub("\\.Rdata$", "", sv_file, ignore.case=TRUE)
	obase = gsub("-sv$", "", obase, ignore.case=TRUE)
	}
    else {
	if (is.null(sv[['ob']])) obase = sv$obase	# sim.R 0.93 & earlier
	else obase = sv$ob$obase		# sim.R 0.94 2014-05-19 & later
	}

    if (driver == "pdf") {
	if (is.character(file)) {
	    ofile = file
	    }
	else {
	    # Note (n): remove ptag from name; keep the other tags in the name.
	    # That is, given a name of the form aa-bb-cc-dd-ee, convert it to
	    # aa-bb-cc--ee (deliberately making a double dash "--").
	    # If there is no -dd or -ee part of name, they are ignored.
	    fbase = gsub("([^-]*-[^-]*-[^-]*-)[^-]*(.*)", "\\1\\2", obase)
	    fbase = paste0(fbase, "-plotIC")
	    ofile = paste0(fbase, ".pdf")
	    if (!overwrite && file.access(ofile, 0) == 0) {
		N = 1
	        while (TRUE) {
		    ofile = paste0(fbase, N, ".pdf")
		    if (file.access(ofile, 0) != 0) break;
		    N = N+1
		    }
		}
	    }
	if (file.access(dirname(ofile), 2) != 0)
	    stop(paste("Cannot create a PDF file in ", dirname(ofile), ".  Specify a valid file= value.", sep=''))
        pdf(ofile, width=20, height=7)
	}

    m_in = m; # might need below
    dynam = (!is.null(sv$sc$bw))
    if (is.null(titl1)) {
	# See note (n) above
	titl1 = gsub("([^-]*-[^-]*-[^-]*-)[^-]*(.*)", "\\1\\2", basename(obase))
	}
    # nroll_null: use this for startup
    if (!is.null(sv$sc) && !is.null(sv$sc$par$dssn)) nroll_null = sv$sc$par$dssn
    else nroll_null = 24;
    if (is.null(nroll)) nroll = nroll_null;
    if (!is.null(p)) means = p;
    if (is.null(means)) {
	if (!is.null(sv$sc) && !is.null(sv$sc$par$dssp)) means = sv$sc$par$dssp
	else means = "tri";
	}
    if (is.null(startup)) startup = nroll_null;

    if (!is.null(sv$sc$par$sfu))
    	scoreu = NA
    else
    	scoreu = sv$sc$par$scoreu # for sm="none": treated just like a "score*"

    ic = (if (is.null(sv$cd.ic)) sv$cd$ic else sv$cd.ic);
    if (is.null(sf)) {
	if (is.null(m)) {
	    if (!is.null(sv$sc$par$sfu)) {
		# has sfu
		sf = sv$sc$par$sfu
		if (is.null(sv$scorepar$tagN))
		    sf = c(sf, sv$scorepar$tag) # score
		}
	    else { # this should be the case sm="none"
		# has scoreu
		factag = sv$pre$factag
		sf = c(factag[factag != scoreu], scoreu)
		# scoreu == score if no tagN
		}
	    if (!is.null(sv$scorepar$tagN)) {
		if (scoreI) sf = c(sf, sv$scorepar$tag)
		sf = c(sf, sv$scorepar$tagN)
		}
	    if (!is.null(static.score) && static.score %in% colnames(ic)) {
		sf = c(sf, static.score)
		have_static.score = TRUE
		}
	    }
	else {
	    if ("character" %in% apply(m, 2, class))
	    	stop("m must be a matrix of numbers or convertable to one.")
	    sf = colnames(m);
	    }
	}
    else if (!is.null(m)) {
    	m = m[,sf] # extract sf columns of m
	if (!is.null(static.score) && static.score %in% colnames(m_in) && !(static.score %in% sf)) {
	    m = cbind(m, m_in[,static.score])
	    have_static.score = TRUE
	    }
	}
    if (is.null(m)) {
    	m = ic[,sf, drop=FALSE] # don't want all columns
	}
    ns = ncol(m);
    nt = length(sv$pre$T)
    # at this point, we want iscore to only include scores.
    if (is.na(scoreu))
	iscore = grep("score", sf) # Matches "*score*".  Might be integer(0).
    else
    	iscore = grep(scoreu, sf)

    if (class(porder) != "character" && sort(porder) != seq(ns))
        stop("porder does not match the number of factors/scores to plot")

    if (annual) as = ann_stat(m, dates=as.character(ic$date))
    if (means == "roll") { 
	x = t(m);
	rowm = rowMeans(x, na.rm=TRUE)
	colnames(rowm) = colnames(m)
	}
    else {
        x = as.matrix(m);
	xsmo = matrix(nrow=nt, ncol=ns)
	colnames(xsmo) = colnames(m)
	if (annual) rowm = colMeans(x, na.rm=TRUE)
	}

    nscore = length(iscore)
    score = (nscore > 0);
    nfac = ns - nscore - have_static.score
    if (is.na(maxperplot)) {
        if (colors[1] == "dflt") maxperplot = nfac
	else maxperplot = 6
	}
    ifac = grep("score", sf, invert=TRUE)

    # set colors
    not_colors = iscore
    if (is.null(colors)) {
	ncol = min(nfac, maxperplot);
    	colors = palette()[1+seq(ncol)]
	# NOTE: The non-factor colors will be made black before plotting
	}
    else if (class(colors) == "integer") {
    	colors = palette()[colors]
	}
    if (length(colors) > 1 && class(colors) == "character")
	colors[colors=="yellow"] = "brown"
    if (colors[1] == "rainbow") { # special, used for factors only
	# the hues of the first and last rainbow values are nearly the same
	colors = rainbow(nfac+1)
	}
    else if (colors[1] == "dflt") { # special 2, used for factors only
	# need to be clever about static.score, which is index ns
	if (!is.null(static.score)) not_colors = c(iscore,ns)
	colors = color_grp(sf[-not_colors]);
	}

    # Any colors that are too close to yellow are turned to shades of brown
    if (class(colors[-iscore]) == "character" & all(substr(colors[-iscore],1,1) == "#"))
    	colors[-iscore] = sub_brown(colors[-iscore]);
    colors[not_colors] = "black"
        
    # final coefs of dynscore if created
    # sc$bw[iter,k,1] where k is the factor
    if (dynam) {
	cw = colnames(sv$sc$bw)
	ibeta = match(sf, cw[sv$sc$ibeta])
	ibeta = ibeta[!is.na(ibeta)]
	if (is.null(sv$sc$f_j)) {
	    sv$sc$f_j = b_F_index(sv$sc$bLname)
	    sv$sc$g_k = b_G_index(sv$sc$bLname)
	    }
	b = sv$sc$bw[(startup+1):nt,cw[sv$sc$f_j %in% ibeta], 1]  # incl G's
	# do we want fmin also?
	if (is.null(fmax)) {
	    mb = max(b, na.rm=TRUE)
	    # round it to a multiple of .1
	    mb = ceiling(mb/.1)*.1;
	    }
	else mb = fmax
	minb = min(b, na.rm=TRUE)
	minb = floor(minb/.1)*.1 # is this the correct scale?
	if (mb >= .4) {
	    btick = .05
	    blab = .2
	    }
	else {
	    btick = .025
	    blab = .1
	    }
	if (minb >= .4) {
	    gtick = .05
	    glab = .2
	    }
	else {
	    gtick = .025
	    glab = .1
	    }
	}
    else {
    	btick = blab = mb = b = minb = glab = gtick = NA;
	}
# -------- stuff above here is independent of number of plots created.

    # If number of factors for last page is small, take factors from others.
    # Strategy:
    # Calculate number of pages (plots) according to maxperplot: nch plots
    # But prefer to spread the number of factors per plot evenly: npp per plot
    nch = ceiling(nfac/maxperplot); 
    npp = ceiling(nfac/nch); # guarantees nch pgs; last page has <= npp factors
    # NOTE: If willing to change npp inside the following loop, then could
    # get something like 6,5,5 rather than 6,6,4

    # porder
    porder_opt = porder
    indxp = 1:ns
    if (porder_opt == "f") porder = indxp
    else if (porder_opt == "b") porder = rev(indxp)
    else porder = porder_opt[indxp]

    xlab = sv$pre$T # yyyy-mm-dd
    nt = length(xlab)
    xlab = xlab[(startup+1):nt]
    nmo = length(xlab) # may be less than nt
    x3 = grep("-03-", xlab)
    x6 = grep("-06-", xlab)
    x9 = grep("-09-", xlab)
    x12 = grep("-12-", xlab)
    xat = unique(c(1,x3,x6,x9,x12,nmo)) # unique in case 1 or nt already there.
    first_plot = 1
    for (n in nroll) {
	# smoothing stuff here

	if (means == "roll") {
	    msmooth = apply(x, 1, rollapply, n, mean, na.rm=TRUE)
	    #Mroll <<- msmooth;
	    # NOTE: some of the msmooth's may be thrown away before plotting but
	    # we don't know which ones without a lot of work.  Basically it would
	    # be whichever columns of ic are for scores.
	    }
	else {
	    # I THINK it doesn't matter if startup < n.
	    wt = geomwt(n, means, fwd=TRUE) # 'tri', 'flat', or 0 < p <= 1.
	    for (i in seq(nt)) {
		xsmo[i,-iscore] = ds_smo(wt, x[,-iscore, drop=FALSE], i);
		xsmo[i,iscore] = ds_smo(wt, x[,iscore, drop=FALSE], i, 1);
		}
	    msmooth = xsmo[(startup+1):nt,,drop=FALSE]
	    }

	# set up the plotting region
	if (first_plot) {
	    z = max(abs(msmooth), na.rm=TRUE)
	    #z = ceiling(max(abs(msmooth), na.rm=TRUE)/2.)*2. # multiple of 2.
	    if (is.null(yrange)) yrange = c(-z,z);
	    if (dynam) { # add .25 range(yrange) to yrange[1]
		ydel = .25*(yrange[2]-yrange[1]);
		ydelz = ceiling((z+ydel)/yround)*yround;
		ydel0 = ydelz-z; # "original" delta: use for legend placement
		yrange[1] = -ydelz;
		# NOTES:
		# (1) We round the lowest Y-axis value to yround.  yround also controls
		# spacing of ticks and tick labels on the main Y-axis.
		#
		# (2) We really should truncate the smoothed IC's at the original
		# yrange[1] value for plotting.  But the IC plots are distinct enough
		# from the dynamic coef. plots that we don't bother.

		# ydelz is used to set the new yrange[1].  We want to place the second
		# plot about 1/3 of the NEW range.  So we recalculate ydel:
		ydel = .33*(yrange[2]-yrange[1]);
		ydel2 = ceiling((yrange[1]+ydel)/.2)*.2;
		# NOTE: If change .2 rounding then also have to change the corresponding
		# Y-axis label.
		ydel = ydel2 - yrange[1]; 
		}
	    else {
		yrange[1] = floor(yrange[1]/yround)*yround;
		yrange[2] = -yrange[1];
		ydel0 = 5; # used for legend placement
		ydel = NA;
		}
	    }
	    
	if (score) {
	    what = "Factor and Score"
	    }
	else
	    what = "Factor"
	if (n == 1)
	    titl2 = paste(what, " IC's: ", "Monthly IC's", sep="")
	else {
	    titl2 = paste(what, " IC's: ", n, "-month", sep="")
	    if (means != "roll") {
		titl2 = paste0(titl2, " (p=", means, ")", if (means==1 || means=="flat") "" else " Weighted")
		}
	    titl2 = paste(titl2, "Trailing Means")
	    }
	if (annual) titl2 = paste(titl2, "and Annual Means")
	if (dynam) titl2 = paste(titl1, titl2, "Dynamic Smooth Factor Coefficients", sep="\n")
	else titl2 = paste(titl1, titl2, sep="\n")
	# special line types for scores
	# static.score will be a solid line as long as it's not in iscore
	slty = c("21","83","44") # applied in reverse order
	nsc = length(iscore)
	nlt = length(slty)
	if (nsc > nlt) slty = c(slty,rep(slty[nlt],nsc-nlt))
	lty = rep("solid",ns); lty[iscore] = rev(slty[1:nsc])
	lwd = rep(1,length(lty));
	
	ltyo = rep("dashed",ns); ltyo[iscore] = lty[iscore]
	#lwd[lty=="23"] = 1.5 # tuned for PDF
	lwd[ltyo=="21"] = 1.7 # tuned for PDF -- 1.2 and even 1.5 are not dark enough
	if (score) ltitl = "Factor or Score"
	else ltitl = "Factor"

	# graphics stuff here
	if (driver == "x")
	    x11(width=20, height=7, family="sans")

	plot_it_IC(sv, porder, porder_opt, indxp, nmo, yrange, xat, x12, annual, titl2,
	    colors, means, n, x, msmooth, startup, nt, lty, ltyo, lwd,
	    rowm, as, xlab, ydel, ydel0, sf, ltitl, dynam, scoreu,
	    mb, minb, b, driver, box, btick, blab, gtick, glab, yround,
	    scorediff, fallback.indic, nametrans, first_plot)
	first_plot = 0
	}
    if (driver == "pdf") {
	graphics.off()
	if (!quiet) cat0("plotIC created ", ofile, "\n");
	}
    }

plot_it_IC = function(sv, porder, porder_opt, indxp, nmo, yrange, xat, x12, annual, titl2,
    colors, means, n, x, msmooth, startup, nt, lty, ltyo, lwd,
    rowm, as, xlab, ydel, ydel0, sf, ltitl, dynam, scoreu,
    mb, minb, b, driver, box, btick, blab, gtick, glab, yround, scorediff,
    fallback.indic, nametrans, first_plot)
    {
    par(mar=c(2,4,4,1))
    zz = plot(c(1,nmo), yrange, type="n", axes=FALSE, xlab="", ylab="")
    if (box) box()
    lines(c(1,nmo), c(0,0)) # axis at 0
    axis(side=1, at=xat, labels=FALSE, pos=yrange[1])
    # tick marks point down
    if (fallback.indic && !is.null(sv$sc$mflag)) {
    	mf0 = (sv$sc$devr==0)[(startup+1):nt];
	points((1:nmo)[mf0], rep(yrange[1],sum(mf0)), pch=23, bg="black", cex=.6)
	}
    axis(side=1, at=1:nmo, tcl=-.2, labels=FALSE, pos=yrange[1])
    axis(side=1, at=x12, tcl=-.8, labels=xlab[x12], pos=yrange[1]) # long ticks
    # ticks at multiples of yround, symmetric about 0.
    # labels every 2*yround ticks, including 0.
    aty = seq(yrange[1],yrange[2],yround) # this works because yrange[1] is
    					  # a multiple of yround.
    atl2 = seq(0.,yrange[2],2.*yround)
    atl1 = seq(-2.*yround,yrange[1],-2.*yround)
    atl = c(atl1,atl2)
    atlab = as.character(atl); atlab[atlab=="0"] = "0.0"
    axis(side=2, at=aty, labels=FALSE, tcl=-.35)
    axis(side=2, at=atl, labels=atlab)
    title(titl2, family="sans")

    cm = colnames(msmooth)
    scoreItag = sv$scorepar$tag
    if (scorediff & (scoreItag %in% cm && "score.N" %in% cm)) {
	sdiff = msmooth[,"score.N"] - msmooth[,scoreItag]
	pcol = rainbow(5,.2)[4] # for example
	sdiff2 = c(0,sdiff,0)
	xsdiff = c(1,1:nmo,nmo)
	polygon(xsdiff, sdiff2, col=pcol)
	}

    nlines = length(porder)
    j = 1
    for (i in porder) {
	icol = ifelse(porder_opt == "b", colors[nlines-j+1], colors[j])
        # rolling means
	if (means == "roll") { 
	    if (is.na(x[i,1])) {
		ry = c(rep(NA, n), msmooth[2:nrow(msmooth),i])
		}
	    else {
		ry = c(rep(NA, n-1), msmooth[,i])
		}
	    
	    ry = ry[(startup+1):nt];
	    }
	else ry = msmooth[,i];
        lines(ry, col=icol, lty=lty[i], lwd=lwd[i])
	# overall mean
	if (annual) {
	    lines(c(1,nmo), rep(rowm[i],2), col=icol, lty=ltyo[i], lwd=lwd[i])
	    }
	j=j+1
	}

    # annual means
    if (annual) {
	dates = rownames(as)
	yyyy = unique(substr(xlab,1,4))
	for (y in yyyy) {
	    ij = grep(y, xlab)
	    icc = grep(y, dates)
	    iij = range(ij) # min, max: first, last
	    j = 1
	    for (i in porder) {
		icol = ifelse(porder_opt == "b", colors[nlines-j+1], colors[j])
		lines(iij, rep(as[icc,i],2), col=icol, lty=lty[i])
		j=j+1
		}
	    }
	legend(x=1,y=yrange[2], legend=c("overall mean", "solid horizontal line segments are annual means"), lty=c(2,0))
        }

    if (dynam) {
	cb = colnames(b)
	bindx = match(sf[porder], cb) # NA's are ignored in loop
	bindx = bindx[!is.na(bindx)]
	nb = length(bindx)
	# btick, blab
	tseq = seq(0,mb,btick)
	lseq = seq(0,mb,blab)

	atb = yrange[1]+tseq*ydel/mb
	atb2 = yrange[1]+tseq*ydel*2/mb
	atb2 = atb2[atb2<(yrange[1]+ydel)]
	atl = yrange[1]+lseq*ydel/mb
	atlab = as.character(lseq); atlab[1] = "0.0"
	axis(side=2, at=atb, labels=FALSE, mgp=c(-2,-1.5,-.5), tcl=.2)
	axis(side=2, at=atl, labels=atlab, mgp=c(-2,-1.5,-.5), tcl=.5)
	axis(side=2, at=atb2, labels=FALSE, mgp=c(-2,-1.5,-.5), tcl=.35)
	j = 1
	for (i in bindx) {
	    icc = b[,i]
	    ics = yrange[1]+ydel*icc/mb
	    icol = ifelse(porder_opt == "b", colors[nb-j+1], colors[j])
	    points(ics, col=icol, font=5, pch=20);
	    lines(ics, col=icol);
	    j=j+1
	    }
	}

    # put legend on top of everything
    y = -ydel0*.5
    lgnd = gsub("score.O", "score.I", sf)
    if (nametrans) {
    	li = match(lgnd, name_trans[,1])
	lgnd[!is.na(li)] = name_trans[na.omit(li),2]
	}
    legend2(x=1, y=y, legend=lgnd, col=colors, lty=lty, lwd=lwd, titl1="Factor", titl2=ltitl, maxlines=NA)
    plot_stamp(plotIC_rcs_id)

    
    
    
    
    ngplot = length(sv$sc$imu)-1

    # plot the gammas for these betas
    if (dynam && length(sv$sc$igamma)) {
	cb = colnames(b)
	nb = length(bindx) # same nb as above
	k = 1
	# plot:
	ymin = yrange[1]
	ymax = yrange[2];
	Del = (ymax - ymin)/ngplot;
	yg1 = ymax-Del; # ymax-Del to ymax
	yg2 = ymax
	gscal = (mb-minb)/Del
	goff = yg1
	tseq = seq(minb, mb, gtick) 
	lseq = seq(minb, mb, glab)
	lseqs = lseq/gscal
	lmin = lseqs[1]
	goff = lmin - yg1
	# ticks at multiples of yround, symmetric about 0.
	# labels every 2*yround ticks, including 0.
	aty = seq(yrange[1],yrange[2],yround) # this works because yrange[1] is
	# new page of plots
	if (driver == "x")
	    x11(width=20, height=7, family="sans")
	par(mar=c(2,4,4,1))
	plot(c(1,nmo), yrange, type="n", axes=FALSE, xlab="", ylab="")
	gtitl = "gammas:"

	np = 1;
	lsgn = 1;
	for (j in sv$sc$imu) { # not NA
	    if (sv$sc$bSname[j] %in% sv$sc$ggname) {
		# tick marks point down
		axis(side=1, at=1:nmo, tcl=-.2, labels=FALSE, pos=yrange[1])
		axis(side=1, at=x12, tcl=-.8, labels=xlab[x12], pos=yrange[1]) # long ticks
		atb = tseq/gscal - goff
		atb2 = atb + Del # ???
		atb2 = atb2[atb2<yg2]
		atl = lseqs # scaled
		atl = atl - goff
		atlab = as.character(lseq);
		axis(side=2, at=atb, labels=FALSE, mgp=c(-2,-1.5,-.5), tcl=.2*lsgn)
		axis(side=2, at=atl, labels=atlab, mgp=c(-2,-1.5*lsgn,-.5), tcl=.5*lsgn)
		axis(side=2, at=atb2, labels=FALSE, mgp=c(-2,-1.5,-.5), tcl=.35*lsgn)
		lsgn = -lsgn
		# k must be the index of sv$sc$imu
	    	bgprint = (sv$sc$g_k %in% k & sv$sc$f_j %in% bindx)
		bgprint[sv$sc$imu] = FALSE
		#bgprint = bgprint[-sv$sc$imu] # to match columns of b
		llab = paste(sv$sc$bSname[bgprint], collapse=" ")
		gtitl = c(gtitl, llab)
		icc = b[,bgprint[-sv$sc$imu]] # try to use matplot
		ics = icc/gscal - goff
		#icol = ifelse(porder_opt == "b", colors[(nb+1):2], colors[2:(nb+1)])
		if (porder_opt == "b") icol = colors[nb:1] else icol = colors[1:nb]
		matpoints(ics, col=icol, font=5, pch=20)
		matlines(ics, col=icol, font=5, pch=20)
		lines(c(1,nmo),c(-goff,-goff),col="black")


		yg1 = yg1 - Del; # next plot down
		goff = lmin - yg1
		yg2 = yg2 - Del; # next plot down
		np = np+1;
		}
	    k = k+1
	    }
	title(paste(gtitl, sep="\n"), family="sans")
	legend2(1,0, legend=sv$sc$par$sfu[bindx], col=colors, lty=1)
    	}
    }

# - - - - - - - - - - - - - - -

# try plotIC().  If fails, error message to std output, and continue.

plotIC.try = function(..., .quiet=FALSE) {
    x = try(plotIC(...), silent=TRUE)
    if (!.quiet && class(x) == "try-error")
    	cat0("ERROR in plotIC() :\n", x[1])	# to stdout
    return(invisible(x))
    }

# - - - - - - - - - - - - - - -

# Wrapper loop for plotIC() to make PDF outputs, for use by rPlotIC.sh.
# Operates on *-sv.Rdata files.  Multiple *-sv.Rdata files are allowed:
#   - It loops on them.
#   - They are specified by putting into a single space-separated string
#     (this is for convenience of calling from sh) not a comma-separated
#     arg list.
# The names are passed to sv.load.chk(); see note (a) above.

plotIC.wrap = function(
    s=NULL,	# sv files in form of white-space separated list
		# (mainly for use when called via bash)
    ...		# other args passed through to plotIC()
    )
    {
    if (!is.null(s)) {
	files = unlist(strsplit0(s, "\\s+"))	# file list (vector)
	}
    for (f in files) {
	x = plotIC.try(f, driver="pdf", ...)
	if (grepl("error", class(x)[1], ig=TRUE))
	    cat0("plotIC failed for ", f, "\n");
	}
    }


# - - - - - - - - - - - - - - -

plotsub_rcs_id = "$Id: plotsub.R,v 1.009 2015-07-15 11:29:48-07 fran PCM $";

# Support routines for graphics

# assign colors to groups of items
# colors come from equally spaced hex colors in sections of the color wheel
# ("rainbow" colors).  Sections are defined by rainbow_bnds below.
# items are grouped by the first letter of the item name.
# possible groups are: 'e', 'g', 'v', 'p'/'q', everything else.

# v is the vector of items to assign colors to.
color_grp = function(v) {
	nv = length(v)
	v1 = sapply(v, function(x){substr(x,1,1)})
	fac_e = (v1=='e'); n_e = sum(fac_e)
	fac_g = (v1=='g'); n_g = sum(fac_g)
	fac_v = (v1=='v'); n_v = sum(fac_v)
	fac_pq = (v1=='p' | v1=='q'); n_pq = sum(fac_pq)
	fac_oth = !(fac_e|fac_g|fac_v|fac_pq); n_oth = sum(fac_oth)

	colors = rep(1,nv); # 1 is arbitrary
	colors[fac_e]   = rainbow_se(n_e,  rainbow_bnds['e',])
	colors[fac_g]   = rainbow_se(n_g,  rainbow_bnds['g',])
	colors[fac_pq]  = rainbow_se(n_pq, rainbow_bnds['pq',])
	colors[fac_oth] = rainbow_se(n_oth,rainbow_bnds['oth',])
	if (rainbow_bnds_oth_brown)
	    colors[fac_oth] = sub_brown2(colors[fac_oth], all=TRUE)
	colors[fac_v]   = rainbow_se(n_v,  rainbow_bnds['v',])
	return(colors)
	}

rainbow_bnds_col = c('e','g','pq','oth','v')
rainbow_dimn = list(rainbow_bnds_col, NULL)
rainbow_bnds0 = matrix(ncol=2, byrow=TRUE, c(
    .95,.05, # e : red (both sides of pure red)
    .12,.24, # g : yellow-orange to yellow-green; replace some with BROWNS
    .76,.90, # pq : purple to magenta
    .29,.44, # oth : pale green to cyanish-green
    .49,.70),  # v : cyan-blue-indigo
    dimnames = rainbow_dimn
    );
rainbow_bnds1 = matrix(ncol=2, byrow=TRUE, c(
    .55,.69, # e : azure-blue-almostindigo
    .75,.88, # g : purple to reddish-magenta
    .11,.19, # pq : orange to yellow : replace some w BROWNS but not .11 or .19
    	     # maybe .19 is too light?  .18 does get changed.
    .97,.06, # oth : red (both sides of pure, but more to orange)
    .37,.50),  # v : green to cyan.
    dimnames = rainbow_dimn
    );
rainbow_bnds2 = matrix(ncol=2, byrow=TRUE, c(
    .565,.685, # e : azure-blue-almostindigo
    .215,.495, # g : yellow-green to cyan.
    .975,.065, # pq : red to orange
    .145,.195, # oth : brown from these bounds
    .745,.880),  # v : violets : purple to (beyond) magenta
    dimnames = rainbow_dimn
    );
rainbow_bnds3 = matrix(ncol=2, byrow=TRUE, c(
    .565,.685, # e : azure-blue-almostindigo
    .215,.520, # g : yellow-green to cyan.
    .970,.070, # pq : red to orange
    .145,.195, # oth : brown from these bounds
    .740,.885),  # v : violets : purple to (beyond) magenta
    dimnames = rainbow_dimn
    );
rainbow_bnds4 = matrix(ncol=2, byrow=TRUE, c(
    .56,.69, # e : azure-blue-almostindigo
    .21,.52, # g : yellow-green to cyan.
    .96,.11, # pq : red to orange
    .03,.21, # oth : browns from these bounds, so can overlap a little
    .70,.90),  # v : violets: indigo to purple to (beyond) magenta
    dimnames = rainbow_dimn
    );

# This next version has the following mnemonics for remembering colors:
# 1. Alphabetic order of the five groups (e, g, pq, oth, v) corresponds to
#    RGB, etc:   reds, greens, blues, browns, violets.
# 2. Growth is green.
# 3. Value is violet.
rainbow_bnds5 = matrix(ncol=2, byrow=TRUE, c(
    .95,.14, # e : red to orange
    .21,.52, # g : yellow-green to cyan.
    .52,.69, # pq : azure-blue-almostindigo
    .03,.21, # oth : browns from these bounds, so can overlap a little
    .72,.90),  # v : violets: indigo to purple to (beyond) magenta
    dimnames = rainbow_dimn
    );

rainbow_bnds = rainbow_bnds5;
rainbow_bnds_oth_brown = TRUE # TRUE if rainbow_bnds['oth',] is browns

rainbow_se = function(n=1, se=c(.1,.4)) {
    return(rainbow_m(n, start=se[1], end=se[2]))
    }

# Replace colors that are too yellow by shades of brown
# defined in HSV coordinates

# In this version, the colors in a vector which meet a yellowness criteria are
# replaced with equally-spaced (numerically) browns whose hue is on
# [brn_hue1, brn_hue2].  Saturation and value are changed to brn_sat and brn_val
brn_hue1 = 15; brn_hue2 = 47 # range of hue
#brn_sat = .75 # saturation
#brn_val = .65 # value
brn_sat = .80 # saturation
brn_val = .80 # value
# originally brn_sat = brn_val = .8
sub_brown = function(col, all=FALSE) {
    h1 = sapply(col, function(x){substr(x,2,3)})
    h2 = sapply(col, function(x){substr(x,4,5)})
    # Hex color values deemed too close to yellow (#FFFF)
    # #XYFF where XY >= E0
    # #FFXY where XY >= D8
    if (!all) {
	y1 = (h1 >= "E0" & h2 == "FF")
	y2 = (h1 == "FF" & h2 >= "D8")
	}
    else {
    	y1 = y2 = rep(TRUE, length(col))
	}
    nch = sum(y1|y2)
    if (nch > 0) {
	hue = brn_hue1+(brn_hue2-brn_hue1+1)*(1:nch)/nch;
	mybrown = hsv(hue/360, brn_sat, brn_val, 1)
	col[y1|y2] = mybrown
	}
    return(col)
    }

# In this version, the hue of colors that meet the yellowness criteria is
# kept but saturation and value are changed to sat and val.
# sat = val = .80 is dma's original choice
sub_brown2 = function(col, sat=.8, val=NULL, all=FALSE) {
    # Hex color values deemed too close to yellow (#FFFF)
    # #XYFF where XY >= E0
    # #FFXY where XY >= D8
    if (is.null(val)) val = sat
    if (!all) {
	h1 = sapply(col, function(x){substr(x,2,3)})
	h2 = sapply(col, function(x){substr(x,4,5)})
	y1 = (h1 >= "E0" & h2 == "FF")
	y2 = (h1 == "FF" & h2 >= "D8")
	}
    else {
    	y1 = y2 = rep(TRUE, length(col))
	}
    if (sum(y1|y2) > 0) {
    	y = col[y1|y2]
	cr = col2rgb(y)
	rh = rgb2hsv(cr["red",], cr["green",], cr["blue",])
	col[y1|y2] = hsv(rh["h",], sat, val)
	}
    return(col)
    }

plot_stamp = function(id, xdel=0.) {
    title = paste(format(Sys.time(), "%F %R %Z   ", tz=TZ_STAMP), gsub("(\\$|(Id: ))", "", id))
    #mtext(title, side=4, line=.5)
    #text(par("usr")[2]+0.25,5.5,srt=180,adj=0,labels=title,xpd=NA)
    par_usr = par("usr")
    ymid = par_usr[3] + (par_usr[4]-par_usr[3])*.5
    text(par_usr[2]+xdel, ymid, srt=-90, adj=.5, labels=title, xpd=NA, cex=.6)
    }

plotAP_stamp = function(id) {
    plot_stamp(id,4.2);
    }

# legend2 -- two-column legend

# legend titles:
# if titl2 is non-NULL, always use it for the last (possibly only) legend
# else use titl1
# if there are two legends, use titl1 for the first unless it's NULL
# In either case, if the preferred title is NULL, use the other one.
# If both are NULL then there is no title.
# return the list returned by the last legend() command with del added

legend2 = function(
	x, y,	# upper lefthand coords for first block
	legend, # same as for legend()
	col,    # same as for legend()
	lty, lwd=1, # same as for legend().  Either of length 1 or length(legend)
	titl1=NULL, titl2=NULL, # see above
	maxlines=7, # max number of lines for first block
		# if NA, maxlines is set to ceiling(length(legend)/2)
	del=.5) # horizontal separation between the two columns
	{
	nent = length(legend);
	if (is.na(maxlines)) maxlines = ceiling(nent/2)
	nfirst = if (nent > maxlines) maxlines else nent
	if (length(lty) == 1) lty = rep(lty,nent)
	if (length(lwd) == 1) lwd = rep(lwd,nent)

	if (nent > maxlines) titl1 = if (!is.null(titl1)) titl1 else titl2
	else titl1 = if (!is.null(titl2)) titl2 else titl1

	# first legend
	l1 = legend(x, y, legend[1:nfirst], col=col[1:nfirst],
		lty=lty[1:nfirst], lwd=lwd[1:nfirst], title=titl1);

	# second legend (if two legends)
	if (nent > maxlines) {
	    # del is the space between the two legends.  Obviously
	    # it depends on the scale of the x-axis.
	    xnew = l1$rect$w+x+del;
	    title = if (!is.null(titl2)) titl2 else titl1
	    nleft = nent-maxlines;
	    l1 = legend(xnew, y, legend[-(1:nfirst)], col=col[-(1:nfirst)],
		lty=lty[-(1:nfirst)], lwd=lwd[-(1:nfirst)], title=title)
	    }
	l1$del = del;
	return(l1)
	}

# tests of colored and dotted lines
# uses same coloring scheme as plotIC
# Default: colored solid lines
# slty non-FALSE: dotted lines (in black&white by default)
sinplot = function(
    v=c('e1','e2','g1','g2','p','q','t1','t2','v1','v2'), # items
    slty=FALSE, # if slty TRUE, set slty to slty_dflt
    bw=NULL, # Black/white: yes if 1, no if 0.
    	     # Default: if slty not FALSE, set to 1, else set to 0
    file=NULL) # use color_grp() to assign colors to groups of items
    {
    slwd = 1
    # See the two sections of help("par") on lty for valid choices
    slty_dflt=c("83","43","42","22","21","22","21","22","21","1343","2262")
    slwd_dflt=c(1,    1,   1,   1,   1,   1.5, 1.5, 1.2,1.2, 1,     1)
    if ((length(slty)>1 || slty) && is.null(bw)) bw=1
    # slty
    if (is.logical(slty)) {
    	if (slty) {
	    slty = slty_dflt
	    slwd = slwd_dflt
	    }
	else {
	    slty = 1
	    if (is.null(bw)) bw = 0
	    }
	}
    # else use slty= option value

    # color
    if (bw)
	colv = "black"
    else # look at v.
	colv = color_grp(v);

    nv = length(v)
    x = seq(0,4*pi,.2)
    nx = length(x)
    maxy = nv*5+1
    y = seq(1,maxy,.2)
    ny = length(y)
    mz = matrix(rep(y,nx), nrow=nx, byrow=TRUE)
    mx = matrix(rep(x,ny), ncol=ny)

    if (length(slty)>1) {
    	slty = rep_len(slty, nv)
	slwd = rep_len(slwd, nv)
	}

    if (is.null(file))
	x11(width=12, height=6, family="sans")
    else
	pdf(file, width=12, height=6, family="sans")
    matplot(mx[,1:length(v)], sin((mx-mz)[,1:length(v)]), type="l", lty=slty, lwd=slwd, col=colv)
    if (bw) # also display line style in legend
	legend2(x=1.5*pi, y=-0.2, legend=paste(v,slty, paste0("l",slwd)), col=rep("black",nv), lty=slty, lwd=slwd)
    else
	legend2(x=1.5*pi, y=-0.2, legend=v, col=colv, lty=slty)
    plot_stamp(plotsub_rcs_id)
    if (!is.null(file)) dev.off()
    }

# simple color wheel with all colors "brownified"
brnpie_cw = function(n, rstart, rend, sat=.80, val=NULL) {
    if (is.null(val)) val = sat;
    col = sub_brown2(rainbow(n, start=rstart, end=rend), sat, val, all=TRUE) 
    pie_cw(rep(1,n), col=col)
    }

# wrapper for pie() with default parameters chosen to give the largest possible
# color wheel of 100 hues with correctly labeled sector midpoints.
pie_cw = function(x=rep(1,100), labels=0:(length(x)-1), radius=1.04, col=rainbow(100), ...) {
    x11();
    pie(x, radius=radius, labels=labels, col=col, ...);
    }

# Modify rainbow() to use the mid-point rule for selecting hues.
# That is, it chooses the midpoints of the n-1 intervals instead of the
# n endpoints.
rainbow_m = function(n, s=1, v=1, start=0, end=max(1, n - 1)/n, alpha = 1) {
	# this only holds for the case start < end but it's the most common
	# case for our needs.
	if (start < 0 || start > 1) start = start %% 1;
	if (end < 0   || end > 1)   end = end %% 1
	if ((n <- as.integer(n[1L])) > 0) {
	    if (start == end || any(c(start, end) < 0) || any(c(start,end) > 1))
		stop("'start' and 'end' must be distinct and in [0, 1].")
	    m = 2*n+1
	    if (start > end) end = end+1
	    hue = seq.int(start, end, length.out=m) %% 1
	    colors = hsv(hue[seq(2,m,2)], s, v, alpha);
	    return(colors)
	    }
	else return(character())
	}

name_trans <- matrix(ncol=2, byrow=TRUE,
	scan(quiet=TRUE, what="character", tc<-textConnection("

other_a tech_a
pROE_a prof_a
qDPAYO_a qual_a

")));
close(tc);

choose_driver = function(driver) {
    if (driver == "jpeg") {
	rundr = jpeg
	}
    else if (driver == "pdf") {
	rundr = pdf
	}
    else if (driver == "png") {
	rundr = png
	}
    else { # including x
	rundr = NULL;
	}
    return(rundr)
    }

# cumulative normalized "size" of rainbow_bnds* color bounds
# used by various plotting routines to choose default colors
rbbnds = function(rbm=rainbow_bnds) {
    nr = nrow(rbm)
    rbl = c()
    for (i in seq(nr)) {
	if (rbm[i,1] > rbm[i,2]) l = rbm[i,2]+(1-rbm[i,1])
	else l = rbm[i,2]-rbm[i,1]
    	rbl = c(rbl, l)
	}
    sl = sum(rbl)
    l = rbl/sl
    csl = cumsum(l)
    print("norm l"); print(l)
    print("rbbnds: cumsum"); print(csl)
    }
