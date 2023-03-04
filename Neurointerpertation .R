library(devtools)
library(oro.nifti)
library(fslr)
library(AnalyzeFMRI)
library(oro.dicom)
library(magrittr)
library(ANTs)
library(basemaps)
#options(fsl.path = "/usr/local/fsl/5.0")
#options(fsl.outputtype = "NIFTI_GZ")
#devtools::install_github("muschellij2/fslr")
#setwd("~/C: /Users/Mustafa/Desktop/neurohacking/5.0")
##-------------------------basic data manupulation 
url <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001-01-MPRAGE.nii.gz"
destfile <- "SUBJ0001-01-MPRAGE.nii.gz"
fname <- file.path(getwd(), destfile)
download.file(url, destfile,mode="wb") # NIfTI is binaryfile format
maskurl <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001_mask.nii.gz"
maskdestfile <- "SUBJ0001_mask.nii.gz"
maskfname <- file.path(getwd(), maskdestfile)
download.file(maskurl, maskdestfile,mode="wb") # NIfTI is binaryfile format
T1 <- readNIfTI(fname,reorient=FALSE)
mask <- readNIfTI(maskfname, reorient=FALSE)
print (T1)
print (mask)
orthographic(T1)
orthographic(mask)
masked.T1 <- niftiarr(T1, T1*mask)
orthographic(masked.T1)
followurl <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001-02-MPRAGE.nii.gz"
followdestfile <- "SUBJ0001-02-MPRAGE.nii.gz"
followfname <- file.path(getwd(), followdestfile)
download.file(followurl, followdestfile,mode="wb")
T1.follow <- readNIfTI(followfname, reorient=FALSE)
subtract.T1 <- niftiarr(T1, T1.follow - T1)
orthographic(subtract.T1)
baseurl <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001-01-MPRAGE_N3.nii.gz"
basefile <- "SUBJ0001-01-MPRAGE_N3.nii.gz"
basefname <- file.path(getwd(), basefile)
download.file(baseurl, basefile,mode="wb")
followurl <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001-02-MPRAGE_N3_REG.nii.gz"
followfile <- "SUBJ0001-02-MPRAGE_N3_REG.nii.gz"
followfname <- file.path(getwd(), followfile)
download.file(followurl, followfile,mode="wb")
T1.base.process <- readNIfTI(basefname, reorient=FALSE)
T1.follow.process <- readNIfTI(followfname, reorient=FALSE)
subtract.T1.process <- niftiarr(T1, T1.follow.process - T1.base.process)
orthographic(subtract.T1.process)
##-------------Visualizing an Image from DICOM file
url <- "https://raw.githubusercontent.com/muschellij2/Neurohacking_data/master/BRAINIX/DICOM/T1/IM-0001-0001.dcm"
destfile <- "IM-0001-0001.dcm"
name <- file.path(getwd(), destfile)
download.file(url, destfile,mode="wb") # DICOM is binaryfile format
dcm <- readDICOMFile(destfile)
extractHeader(dcm$hdr, "Manufacturer", numeric=FALSE)
image(t(dcm$img), col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
      main="Example image from DICOM file")
##-_____________Visualizing Images from NIfTI file
url <- "https://raw.githubusercontent.com/muschellij2/Neurohacking_data/master/BRAINIX/NIfTI/Output_3D_File.nii.gz"
destfile <- "Output_3D_File.nii.gz"
name <- file.path(getwd(), destfile)
download.file(url, destfile,mode="wb") # NIfTI is binaryfile format
nii_T1 <- readNIfTI(destfile)
print (nii_T1)
image(nii_T1,z=11,plot.type="single")
image(nii_T1)
##_____________________Transformations and Smoothing
url <- "https://raw.githubusercontent.com/muschellij2/Neurohacking/master/Basic_Data_Manipulations/Kirby21/SUBJ0001-01-MPRAGE.nii.gz"
destfile <- "Output_3D_File.nii.gz"
name <- file.path(getwd(), destfile)
download.file(url, destfile,mode="wb") # NIfTI is binaryfile format
T1 <- readNIfTI(destfile)
image(T1,z=11,plot.type="single")
im_hist<-hist(T1,plot=FALSE)
par(mar = c(5, 4, 4, 4) + 0.3)
col1=rgb(0,0,1,1/2)
plot(im_hist$mids,im_hist
     $count,log="y",type='h',lwd=10, lend=2,
     col=col1,xlab="Intensity Values",ylab="Count
(Log Scale)" )
im_hist<-hist(T1,plot=FALSE)
par(mar = c(5, 4, 4, 4) + 0.3)
col1=rgb(0,0,1,1/2)
plot(im_hist$mids,im_hist
     $count,log="y",type='h',lwd=10, lend=2,
     col=col1,xlab="Intensity Values",ylab="Count
(Log Scale)" )
par(new = TRUE)
curve(x*1, axes = FALSE,xlab = "",ylab= "",
      col=2, lwd=3)
axis(side=4,at = pretty(range(im_hist$mids))/
       max(T1), labels=pretty(range(im_hist$mids)))
mtext("Original Intensity", side=4, line=2)
im_hist<-hist(T1,plot=FALSE)
par(mar = c(5, 4, 4, 4) + 0.3)
col1=rgb(0,0,1,1/2)
plot(im_hist$mids,im_hist
     $count,log="y",type='h',lwd=10, lend=2,
     col=col1,xlab="Intensity Values",ylab="Count
     (Log Scale)" )
par(new = TRUE)
curve(x*1, axes = FALSE,xlab = "",ylab= "",
      col=2, lwd=3)
axis(side=4,at = pretty(range(im_hist$mids))/
       max(T1), labels=pretty(range(im_hist$mids)))
mtext("Original Intensity", side=4, line=2)
curve(lin.sp(x,knot.vals,slp.vals),axes=FALSE,xlab="",ylab="",col=2,lwd=3)
axis(side=4,at = pretty(range(im_hist$mids))/
       max(T1),labels=pretty(range(im_hist$mids)))
mtext("Transformed Intensity", side=4, line=2)
trans_T1<-lin.sp(T1, knot.vals*max(T1), slp.vals)
image(T1,z=11,plot.type='single', main="Original Image")
image(trans_T1,z=11,plot.type='single',main="Transformed Image")
smooth.T1 <- GaussSmoothArray(T1,voxdim=c(1,1,1),ksize=1,sigma=diag(3,3),mask=NULL,var.norm=FALSE)
orthographic(smooth.T1)
##----------Pre-Processing Neurohacking <problem .... >
options(fsl.path= "/usr/local/fsl") # fsl-complete install 2 version 4.1 and 5.0 on ubuntu/debian
library(oro.nifti)
library(fslr)
destfile <- "113-01-MPRAGE.nii.gz"
if(!file.exists(destfile))
{
  url <- "https://raw.githubusercontent.com/muschellij2/Neurohacking_data/master/kirby21/visit_1/113/113-01-MPRAGE.nii.gz"
  name <- file.path(getwd(), destfile)
  download.file(url, destfile,mode="wb") # NIfTI is binaryfile format
}
nim=readNIfTI("113-01-MPRAGE.nii.gz", reorient=FALSE)
Sys.setenv("LD_LIBRARY_PATH"="/usr/local/lib/") # R process maybe ignore LD_LIBRARY_PATH so i set it in code
mean(nim)
#export LD_LIBRARY_PATH=/usr/lib/fsl/5.0:$LD_LIBRARY_PATH
fslstats(nim , opts= "-m")
fslstats.help()
fslstats("113-01-MPRAGE.nii.gz",opts="-m")
fast_img = fsl_biascorrect(nim, retimg=TRUE)
orthographic(nim)
orthographic(fast_img)
library(scales)
sub.bias <- niftiarr(nim, nim-fast_img)
# quantile the difference image using these as breaks
q=quantile(sub.bias[sub.bias !=0],probs = seq(0,1,by=0.1))
# get a diverging gradient palette
fcol=div_gradient_pal(low="blue",mid="yellow",high ="red")
ortho2(nim,sub.bias,col.y = alpha(fcol(seq(0,1, length=10)),
                                  0.5), ybreaks = q, ycolorbar=TRUE, text = paste0("Original
Image Minus N4", "\n Bias-Corrected Image"))
library(ggplot2)
library(reshape2)
slices = c(2, 6, 10, 14, 18)
vals = lapply(slices, function(x) {
  cbind(img = c(nim[,,x]), fast = c(fast_img[,,x]),
        slice = x)
})
vals = do.call("rbind", vals)
vals = data.frame(vals)
vals = vals[ vals$img > 0 & vals$fast > 0, ]
colnames(vals)[1:2] = c("Original Value", "Bias-Corrected Value")
v = melt(vals, id.vars = "slice")
g = ggplot(aes(x = value, colour = factor(slice)), data = v) + geom_line(stat = "density") + facet_wrap(~ variable)
g = g + scale_colour_discrete(name = "Slice #")
print(g)
bet_fast = fslbet(infile=fast_img, retimg=TRUE)
bet_fast_mask <- niftiarr(bet_fast, 1)
is_in_mask = bet_fast>0
bet_fast_mask[!is_in_mask]<-NA
orthographic(bet_fast)
orthographic(fast_img,bet_fast_mask)
cog = cog(bet_fast, ceil=TRUE)
cog = paste("-c", paste(cog, collapse= " "))
bet_fast2 = fslbet(infile=fast_img,retimg=TRUE,opts=cog)
orthographic(bet_fast2)
dim(bet_fast2)
##$$$$=============================
Sys.getenv("FSLDIR")
library(fslr) 
have.fsl() 
##----------------------------------
library(oro.nifti) 
library(extrantsr) 
library(fslr) 
kirbydir <- "/home/fsluser/Desktop/MOOC-2015/kirby21" 
mridir=file.path(kirbydir, "visit_1", "113") 
T1_file=file.path(mridir, "113-01-MPRAGE.nii.gz") 
T1=readNIfTI(T1_file,reorient=FALSE) 
T2_file=file.path(mridir, "113-01-T2w.nii.gz") 
T2w=readNIfTI(T2_file) 
flirt_reg_t2_img = flirt(infile = T2_file, reffile = 
                           T1,dof = 6,verbose = FALSE) 
double_ortho(T1, flirt_reg_t2_img)
dim(T1)
dim(flirt_reg_t2_img)
dim(T2w) 
T2_file=file.path(mridir, "113-01-T2w.nii.gz") 
reg_t2_img = ants_regwrite(filename = T2_file, 
                           template.file=T1,typeofTransform="Rigid",verbose= FALSE) 
flair_file=file.path(mridir, "113-01-FLAIR.nii.gz") 
reg_flair_img = ants_regwrite(filename = flair_file, 
                              template.file=T1,typeofTransform="Rigid",verbose= FALSE)
double_ortho(T1, reg_t2_img)
library(scales) 
ortho2(T1, reg_t2_img, col.y = alpha(hotmetal(), 0.25)) 
double_ortho(T1, reg_flair_img)
library(scales) 
ortho2(T1, reg_flair_img, col.y = alpha(hotmetal(), 0.25)) 
files = c("113-01-MPRAGE.nii.gz", 
          "113-01-T2w.nii.gz", 
          "113-01-FLAIR.nii.gz") 
files = file.path(mridir, files) 
outfiles = c("113-01-MPRAGE_processed.nii.gz", 
             "113-01-T2w_processed.nii.gz", 
             "113-01-FLAIR_processed.nii.gz") 
outfiles = file.path(mridir, outfiles) 
preprocess_mri_within(files = files, retimg = FALSE, 
                      outfiles = outfiles,correction = "N4",skull_strip = FALSE) 
brain = fslbet_robust(img = outfiles[1], 
                      correct = FALSE, verbose = FALSE) 
mask = brain > 0 
masked_imgs = lapply(outfiles, fslmask, 
                     mask = mask,verbose = FALSE) 
orthographic(masked_imgs[[2]])
mridir2=file.path(kirbydir, "visit_2", "113") 
files2 = c("113-02-MPRAGE.nii.gz", 
           "113-02-T2w.nii.gz", 
           "113-02-FLAIR.nii.gz") 
files2 = file.path(mridir2, files2) 
outfiles2 = c("113-02-MPRAGE_processed.nii.gz", 
              "113-02-T2w_processed.nii.gz", 
              "113-02-FLAIR_processed.nii.gz") 
outfiles2 = file.path(mridir2, outfiles2) 
preprocess_mri_within(files = files2, retimg = FALSE, 
                      outfiles = outfiles2,correction = "N4",skull_strip = FALSE) 
brain2 = fslbet_robust(img = outfiles2[1], 
                       correct = FALSE, verbose = FALSE) 
mask2 = brain2 > 0 
masked_imgs2 = lapply(outfiles2, fslmask, 
                      mask = mask2,verbose = FALSE) 
orthographic(masked_imgs2[[3]])
visit2_files = file.path(mridir2, 
                         c("113-02-MPRAGE_processed.nii.gz", 
                           "113-02-T2w_processed.nii.gz", 
                           "113-02-FLAIR_processed.nii.gz")) 
outfiles2 = sub(".nii.gz", "_reg.nii.gz", visit2_files) 
ants_regwrite(filename = masked_imgs2[[1]], 
              retimg = FALSE, outfile = outfiles2[1], 
              template.file = masked_imgs[[1]], 
              other.files = masked_imgs2[2:3], 
              other.outfiles = outfiles2[2:3], 
              typeofTransform = "Rigid", verbose = FALSE)
ss_t1=masked_imgs[[1]] 
visit_2_t1=readNIfTI(outfiles2[1],reorient=FALSE) 
double_ortho(ss_t1,visit_2_t1) 
ortho2(ss_t1,visit_2_t1,col.y=alpha(hotmetal(),0.25)) 
visit2_files_skull = file.path(mridir2, 
                               c("113-02-MPRAGE_processed.nii.gz", 
                                 "113-02-T2w_processed.nii.gz", 
                                 "113-02-FLAIR_processed.nii.gz")) 
outfiles2_skull = sub(".nii.gz", "_reg.nii.gz", 
                      visit2_files_skull) 
ants_regwrite(filename = visit2_files_skull[[1]], 
              retimg = FALSE, outfile = outfiles2_skull[1], 
              template.file = outfiles[[1]], 
              other.files = visit2_files_skull[2:3], 
              other.outfiles = outfiles2_skull[2:3], 
              typeofTransform = "Rigid", verbose = FALSE)
t1_skull=readNIfTI(outfiles[1],reorient=FALSE) 
v2_t1_skull=readNIfTI(outfiles2_skull[1],reorient=FALSE) 
double_ortho(t1_skull, v2_t1_skull) 
ortho2(t1_skull, v2_t1_skull,col.y=alpha(hotmetal(),0.25)) 
library(oro.nifti) 
library(extrantsr) 
library(fslr) 
library(scales) 
neurodir <- "/home/fsluser/Desktop/MOOC-2015" 
mridir = file.path(neurodir, "BRAINIX", "NIfTI") 
t1 = file.path(mridir, "T1.nii.gz") 
t1 = readNIfTI(t1, reorient = FALSE) 
flair = file.path(mridir, "FLAIR.nii.gz") 
roi = file.path(mridir, "ROI.nii.gz") 
flair_file=readNIfTI(flair, reorient = FALSE) 
roi_file=readNIfTI(roi, reorient = FALSE) 
is_tumor<- (roi_file>0) 
roi_file[!is_tumor]=NA 
orthographic(flair_file,roi_file, xyz=c(200,155,12), 
             col.y=alpha("red",0.2), 
             text="Image overlaid with mask", 
             text.cex = 1.5)
reg_flair = file.path(mridir,"FLAIR_regToT1.nii.gz") 
reg_roi = file.path(mridir,"ROI_regToT1.nii.gz") 
reg_flair_img = ants_regwrite(filename = flair, 
                              template.file = t1, 
                              outfile = reg_flair, 
                              typeofTransform = "Rigid", 
                              other.files = roi, 
                              other.outfiles = reg_roi, 
                              verbose = FALSE) 
reg_roi_img = readNIfTI(reg_roi, reorient = FALSE)
double_ortho(t1,reg_flair_img)
ortho2(reg_flair_img,reg_roi_img,col.y=alpha("red",0.2))
n4_t1 = bias_correct(t1, correction = "N4") 
brain = fslbet_robust(img = n4_t1, 
                      correct = FALSE, verbose = FALSE) 
template.file = file.path(neurodir, 
                          "Template","JHU_MNI_SS_T1_brain.nii.gz") 
aff_t1_outfile = file.path(mridir,"T1_AffinetoEve.nii.gz") 
aff_roi_outfile = file.path(mridir, 
                            "ROI_regToT1_AffinetoEve.nii.gz") 
aff_brain = ants_regwrite(filename = brain, 
                          outfile = aff_t1_outfile, 
                          other.files = reg_roi, 
                          other.outfiles = aff_roi_outfile, 
                          template.file = template.file, 
                          typeofTransform = "Affine", 
                          verbose = FALSE) 
aff_roi = readNIfTI(aff_roi_outfile, reorient = FALSE)
template = readNIfTI(template.file, reorient= FALSE) 
double_ortho(aff_brain, template)
ortho2(aff_brain, template,col.y=alpha(hotmetal(),0.35))
ortho2(aff_brain, template,z=ceiling(dim(template)[3]/2),  plot.type= "single",col.y=alpha(hotmetal(),0.35))
ortho2(aff_brain, aff_roi,col.y=alpha(hotmetal(),0.35), 
       xyz=xyz(aff_roi))
template.file = file.path(neurodir, 
                          "Template","JHU_MNI_SS_T1_brain.nii.gz") 
syn_t1_outfile = file.path(mridir,"T1_SyntoEve.nii.gz") 
syn_roi_outfile = file.path(mridir, 
                            "ROI_regToT1_SyntoEve.nii.gz") 
syn_brain = ants_regwrite(filename = brain, 
                          outfile = syn_t1_outfile, 
                          other.files = reg_roi, 
                          other.outfiles = syn_roi_outfile, 
                          template.file = template.file, 
                          typeofTransform = "SyN"
, 
 verbose = FALSE) 
syn_roi = readNIfTI(aff_roi_outfile, reorient = FALSE)
double_ortho(syn_brain, template)
ortho2(syn_brain, template, col.y=alpha(hotmetal(),0.35))
ortho2(syn_brain, template,z=ceiling(dim(template)[3]/2), 
       plot.type= "single",col.y=alpha(hotmetal(),0.35))
ortho2(aff_brain, template,z=ceiling(dim(template)[3]/2), 
       plot.type= "single",col.y=alpha(hotmetal(),0.35))
ortho2(syn_brain, syn_roi, col.y=alpha(hotmetal(),0.35), 
       xyz=xyz(syn_roi))
#### extracting JHU Eve atlas Type I and labels########### 
atlas = "JHU_MNI_SS_WMPM_Type-I" 
txtfile = file.path(neurodir, "Template", 
                    paste0(atlas, "_SlicerLUT.txt")) 
### read look up table (LUT) 
jhut1.df = read.table(txtfile, stringsAsFactors=FALSE) 
jhut1.df = jhut1.df[, 1:2] 
colnames(jhut1.df) = c("index", "Label") 
jhut1.df$index = as.numeric(jhut1.df$index)
jhut1.df[1:4,]
## read in the template image 
jhut1.img = readNIfTI(file.path(neurodir, "Template", 
                                paste0(atlas, ".nii.gz"))) 
##Obtain the numeric labels from the atlas 
uimg = sort(unique(c(jhut1.img))) 
##Obtain the numeric labels from the LUT 
all.ind = jhut1.df$index 
##Check that all numeric labels from the atlas are in LUT 
stopifnot(all(uimg %in% all.ind))
hist(c(syn_roi[syn_roi > 0]))
library(plyr) 
##Make a data frame with the index of the atlas 
##and the value of the ROI at that voxel 
roi.df = data.frame(index = jhut1.img[syn_roi> 0], 
                    roi = syn_roi[ syn_roi > 0]) 
##Obtain the number (sum) of voxels that have an roi
##value >0.5 in the roi by the index of labels 
label_sums = ddply(roi.df, .(index), summarize, 
                   sum_roi = sum(roi), sum_roi_thresh = sum(roi > 0.5)) 
label_sums = merge(label_sums, jhut1.df, by="index") 
sums = label_sums # will use later 
# Assign the Labels to the row names 
rownames(label_sums) = label_sums$Label
label_sums$Label = label_sums$index = NULL 
# Reorder labels from the most to the least engaged 
label_sums = label_sums[order(label_sums$sum_roi, 
                              decreasing = TRUE), ] 
# Calculate the percent of the tumor engaging the region 
label_pct = t(t(label_sums)/colSums(label_sums)) * 100 
head(round(label_pct, 1), 10) 
# Create a table that contains the number of voxels 
#engaged in each labeled region 
jhut1.tab = as.data.frame(table(c(jhut1.img))) 
colnames(jhut1.tab) = c("index", "size") 
# Merge the table of number of voxels per region with the 
# table of number of voxels by ROI 
region_pct = merge(sums, jhut1.tab, by="index") 
rownames(region_pct) = sums$Label
# Calculate the percent of region engaged by the tumor 
region_pct$Label = region_pct$index = NULL 
region_pct = region_pct/region_pct$size * 100 
region_pct$size = NULL 
# Reorder regions from the most to the least engaged 
region_pct = region_pct[ order(region_pct$sum_roi, 
                               decreasing = TRUE), ] 
# Calculate the percent of the tumor engaging the region 
head(round(region_pct,1),10) 
mridir = file.path("/home/fsluser/Desktop/MOOC-2015/
kirby21/visit_1/113") 
t1_path=file.path(mridir,"113-01-MPRAGE.nii.gz") 
#Read the file 
nim=readNIfTI(t1_path, reorient=FALSE) 
#Conduct bias field correction 
fast_img = fsl_biascorrect(nim, retimg=TRUE) 
#Perform brain extraction 
bet = fslbet(infile=fast_img, retimg=TRUE) 
#Perform segmentation 
fast=fast(file=bet_fast,outfile=file.path(paste0(mridir,"/
113-01-MPRAGE_biascorrected_BET_FAST.nii.gz"))) 
#Displays CSF segmentation 
ortho2(bet,fast==1,col.y=alpha("red",
                               0.5),text="SUBJ113_CSF_1") 
#Displays GM segmentation 
ortho2(bet,fast==2,col.y=alpha("red",0.5),text="SUBJ113_GM_1") 
#Displays WM segmentation 
ortho2(bet,fast==3,col.y=alpha("red",0.5),text="SUBJ113_WM_1") 
#Reads in the pve file for CSF 
pve_CSF=readNIfTI(paste0(mridir,"/113-01-
                         MPRAGE_N4_BET_FAST_pve_0.nii.gz")) 
#Reads in the pve file for GM 
pve_GM=readNIfTI(paste0(mridir,"/113-01-
                        MPRAGE_N4_BET_FAST_pve_1.nii.gz")) 
#Reads in the pve file for WM 
pve_WM=readNIfTI(paste0(mridir,"/113-01-
                        MPRAGE_N4_BET_FAST_pve_2.nii.gz")) 
#Reads in the pve file for CSF 
threshold=0.33 
#Calculate the product of voxel dimensions (volume) 
vdim_CSF=prod(voxdim(pve_CSF)) 
#Reads in the pve file for WM 
nvoxels_CSF=sum(pve_CSF>threshold) 
#Calculate the volume of CSF in mL 
vol_pveCSF=vdim_CSF*nvoxels_CSF/1000
#CSF volume in mL 
vol_pveCSF
#GM volume in mL 
vol_pveGM
#WM volume in mL 
vol_pveWM
