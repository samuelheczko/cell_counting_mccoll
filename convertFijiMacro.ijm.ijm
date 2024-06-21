run("Bio-Formats Macro Extensions");
run("Bio-Formats Importer", "open= autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
run("RGB Color");
close("\\Others")
dir = getDirectory("");
fileList = getFileList(dir);
output_dir = dir + File.separator + "" + File.separator ;
File.makeDirectory(output_dir)
		ch_nbr = nImages ; 
		for ( c = 1 ; c <= ch_nbr ; c++){
			selectImage(c);
			New_Image_to_Save = getTitle();
			saveAs("ome.tiff", output_dir+New_Image_to_Save);
		}
currentImage = getImageID();
close();
