# Build: Ctrl + Shift + B

abbr.para1 <- function(x,colname=TRUE,unit=FALSE,chem=FALSE){

  if(chem==FALSE){

    if(colname==TRUE & unit==FALSE){
      colnames(x) <- gsub("Skin_Elasticity","DFM",as.character(colnames(x)))
      colnames(x) <- gsub("Gradient_Firmness_and_Skin_Strength","SFM",as.character(colnames(x)))
      colnames(x) <- gsub("Gradient","SFM",as.character(colnames(x))) # Gradient_Firmness_and_Skin_Strength is Gradient in some sheets
      colnames(x) <- gsub("Area_Skin_Strength","AFM",as.character(colnames(x)))
      colnames(x) <- gsub("Linear_Distance_F_D_1to3","LDFM",as.character(colnames(x)))
      colnames(x) <- gsub("Mean_Internal_Firmness","MIF",as.character(colnames(x)))
      colnames(x) <- gsub("Area_Internal_Firmness","AIF",as.character(colnames(x)))
      colnames(x) <- gsub("Graininess","NIP",as.character(colnames(x)))
      colnames(x) <- gsub("Force_Linear_Distance","FLD",as.character(colnames(x)))
      colnames(x) <- gsub("Linear_Distance","LDFM",as.character(colnames(x))) # Linear_Distance_F_D_1to3 is Linear_Distance in some sheets
      colnames(x) <- gsub("Total_Area","AFLD",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_10","YM10",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_20","YM20",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_30","YM30",as.character(colnames(x)))
      colnames(x) <- gsub("Height","Dia",as.character(colnames(x)))
      colnames(x) <- gsub("Wet_Stem_Scar","Ssw",as.character(colnames(x)))
      colnames(x) <- gsub("Stem_Scar_Diameter","Ssd",as.character(colnames(x)))
      colnames(x) <- gsub("Stem_Scar_Tearing","Sst",as.character(colnames(x)))
      colnames(x) <- gsub("T6_T0_cupweight","Wg",as.character(colnames(x)))
      colnames(x) <- gsub("Weight","Wg",as.character(colnames(x)))
      colnames(x) <- gsub("Wrinkle","Wrk",as.character(colnames(x)))
      colnames(x) <- gsub("F_ep","Fep",as.character(colnames(x)))
      colnames(x) <- gsub("F_h","Fh",as.character(colnames(x)))
      colnames(x) <- gsub("F_p","Fp",as.character(colnames(x)))
      colnames(x) <- gsub("F_inner","Fin",as.character(colnames(x)))
      colnames(x) <- gsub("Firmness","F1mm",as.character(colnames(x))) # this needs to come last
      colnames(x) <- gsub("Skin_Strength","FM",as.character(colnames(x))) # this needs to come last
    }

    if(colname==TRUE & unit==TRUE){
      colnames(x) <- gsub("Skin_Elasticity","DFM (mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Gradient_Firmness_and_Skin_Strength","SFM (N/mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Gradient","SFM (N/mm)",as.character(colnames(x))) # Gradient_Firmness_and_Skin_Strength is Gradient in some sheets
      colnames(x) <- gsub("Area_Skin_Strength","AFM (N mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Linear_Distance_F_D_1to3","LDFM",as.character(colnames(x)))
      colnames(x) <- gsub("Mean_Internal_Firmness","MIF (N)",as.character(colnames(x)))
      colnames(x) <- gsub("Area_Internal_Firmness","AIF (N mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Graininess","NIP",as.character(colnames(x)))
      colnames(x) <- gsub("Force_Linear_Distance","FLD",as.character(colnames(x)))
      colnames(x) <- gsub("Linear_Distance","LDFM",as.character(colnames(x))) # Linear_Distance_F_D_1to3 is Linear_Distance in some sheets
      colnames(x) <- gsub("Total_Area","AFLD (N mm)",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_10","YM10 (MPa)",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_20","YM20 (MPa)",as.character(colnames(x)))
      colnames(x) <- gsub("M_TM_30","YM30 (MPa)",as.character(colnames(x)))
      colnames(x) <- gsub("Height","Dia (mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Wet_Stem_Scar","Ssw (%)",as.character(colnames(x)))
      colnames(x) <- gsub("Stem_Scar_Diameter","Ssd (mm)",as.character(colnames(x)))
      colnames(x) <- gsub("Stem_Scar_Tearing","Sst (%)",as.character(colnames(x)))
      colnames(x) <- gsub("T6_T0_cupweight","Wg",as.character(colnames(x)))
      colnames(x) <- gsub("Weight","Wg (g)",as.character(colnames(x)))
      colnames(x) <- gsub("Wrinkle","Wrk",as.character(colnames(x)))
      colnames(x) <- gsub("F_ep","Fep (N)",as.character(colnames(x)))
      colnames(x) <- gsub("F_h","Fh (N)",as.character(colnames(x)))
      colnames(x) <- gsub("F_p","Fp (N)",as.character(colnames(x)))
      colnames(x) <- gsub("F_inner","Fin (N)",as.character(colnames(x)))
      colnames(x) <- gsub("Firmness","F1mm (N)",as.character(colnames(x))) # this needs to come last
      colnames(x) <- gsub("Skin_Strength","FM (N)",as.character(colnames(x))) # this needs to come last
    }

    if(colname==FALSE & unit==TRUE){
      x <- data.frame(lapply(x, function(y) {
        y<-gsub("Skin_Elasticity", "DFM (mm)", y)
        y<-gsub("Gradient_Firmness_and_Skin_Strength","SFM (N/mm)", y)
        y<-gsub("Gradient","SFM (N/mm)", y) # Gradient_Firmness_and_Skin_Strength is Gradient in some sheets
        y<-gsub("Area_Skin_Strength","AFM (N mm)", y)
        y<-gsub("Linear_Distance_F_D_1to3","LDFM", y)
        y<-gsub("Mean_Internal_Firmness","MIF (N)", y)
        y<-gsub("Area_Internal_Firmness","AIF (N mm)", y)
        y<-gsub("Graininess","NIP", y)
        y<-gsub("Force_Linear_Distance","FLD", y)
        y<-gsub("Linear_Distance","LDFM", y) # Linear_Distance_F_D_1to3 is Linear_Distance in some sheets
        y<-gsub("Total_Area","AFLD (N mm)", y)
        y<-gsub("M_TM_10","YM10 (MPa)", y)
        y<-gsub("M_TM_20","YM20 (MPa)", y)
        y<-gsub("M_TM_30","YM30 (MPa)", y)
        y<-gsub("Height","Dia (mm)", y)
        y<-gsub("Wet_Stem_Scar","Ssw (%)", y)
        y<-gsub("Stem_Scar_Diameter","Ssd (mm)", y)
        y<-gsub("Stem_Scar_Tearing","Sst (%)", y)
        y<-gsub("T6_T0_cupweight","Wg", y)
        y<-gsub("Weight","Wg (g)", y)
        y<-gsub("Wrinkle","Wrk", y)
        y<-gsub("F_ep","Fep (N)", y)
        y<-gsub("F_inner","Fin (N)", y)
        y<-gsub("F_h","Fh (N)", y)
        y<-gsub("F_p","Fp (N)", y)
        y<-gsub("Firmness","F1mm (N)", y)
        y<-gsub("Skin_Strength","FM (N)", y)
      }))
    }

    if(colname==FALSE & unit==FALSE){
      x <- data.frame(lapply(x, function(y) {
        y<-gsub("Skin_Elasticity", "DFM", y)
        y<-gsub("Gradient_Firmness_and_Skin_Strength","SFM", y)
        y<-gsub("Gradient","SFM", y) # Gradient_Firmness_and_Skin_Strength is Gradient in some sheets
        y<-gsub("Area_Skin_Strength","AFM", y)
        y<-gsub("Linear_Distance_F_D_1to3","LDFM", y)
        y<-gsub("Mean_Internal_Firmness","MIF", y)
        y<-gsub("Area_Internal_Firmness","AIF", y)
        y<-gsub("Graininess","NIP", y)
        y<-gsub("Force_Linear_Distance","FLD", y)
        y<-gsub("Linear_Distance","LDFM", y) # Linear_Distance_F_D_1to3 is Linear_Distance in some sheets
        y<-gsub("Total_Area","AFLD", y)
        y<-gsub("M_TM_10","YM10", y)
        y<-gsub("M_TM_20","YM20", y)
        y<-gsub("M_TM_30","YM30", y)
        y<-gsub("Height","Dia", y)
        y<-gsub("Wet_Stem_Scar","Ssw", y)
        y<-gsub("Stem_Scar_Diameter","Ssd", y)
        y<-gsub("Stem_Scar_Tearing","Sst", y)
        y<-gsub("T6_T0_cupweight","Wg", y)
        y<-gsub("Weight","Wg", y)
        y<-gsub("Wrinkle","Wrk", y)
        y<-gsub("F_ep","Fep", y)
        y<-gsub("F_inner","Fin", y)
        y<-gsub("F_h","Fh", y)
        y<-gsub("F_p","Fp", y)
        y<-gsub("Firmness","F1mm", y)
        y<-gsub("Skin_Strength","FM", y)
      }))
    }

    return(x)
  }

  if(chem==TRUE){

    if(colname==TRUE & unit==FALSE){
      colnames(x) <- gsub("SSC","TSS",as.character(colnames(x)))
      colnames(x) <- gsub("TA","TA",as.character(colnames(x)))
      colnames(x) <- gsub("acidity","TA",as.character(colnames(x)))
      colnames(x) <- gsub("TotalOA","Total OA",as.character(colnames(x)))
      colnames(x) <- gsub("Quinic","Quinic acid",as.character(colnames(x)))
      colnames(x) <- gsub("Malic","Malic acid",as.character(colnames(x)))
      colnames(x) <- gsub("Shikimic","Shikimic acid",as.character(colnames(x)))
      colnames(x) <- gsub("Citric","Citric acid",as.character(colnames(x)))
      colnames(x) <- gsub("TotalSugar","Total Sugar",as.character(colnames(x)))
      colnames(x) <- gsub("Sucrose","Sucrose",as.character(colnames(x)))
      colnames(x) <- gsub("Glucose","Glucose",as.character(colnames(x)))
      colnames(x) <- gsub("Fructose","Fructose",as.character(colnames(x)))
    }


    if(colname==FALSE & unit==FALSE){
      x <- data.frame(lapply(x, function(y) {
        y<-gsub("SSC","TSS", y)
        y<-gsub("TA","TA", y)
        y<-gsub("acidity","TA", y)
        y<-gsub("TotalOA","Total OA", y)
        y<-gsub("Quinic","Quinic acid", y)
        y<-gsub("Malic","Malic acid", y)
        y<-gsub("Shikimic","Shikimic acid", y)
        y<-gsub("Citric","Citric acid", y)
        y<-gsub("TotalSugar","Total Sugar", y)
        y<-gsub("Sucrose","Sucrose", y)
        y<-gsub("Glucose","Glucose", y)
        y<-gsub("Fructose","Fructose", y)
      }))
    }

    if(colname==TRUE & unit==TRUE){
      colnames(x) <- gsub("SSC","TSS (°Brix)",as.character(colnames(x)))
      colnames(x) <- gsub("TA","TA (%)",as.character(colnames(x)))
      colnames(x) <- gsub("acidity","TA (%)",as.character(colnames(x)))
      colnames(x) <- gsub("TotalOA","Total OA (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Quinic","Quinic acid (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Malic","Malic acid (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Shikimic","Shikimic acid (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Citric","Citric acid (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("TotalSugar","Total Sugar (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Sucrose","Sucrose (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Glucose","Glucose (mg/g DW)",as.character(colnames(x)))
      colnames(x) <- gsub("Fructose","Fructose (mg/g DW)",as.character(colnames(x)))
    }

    if(colname==FALSE & unit==TRUE){
      x <- data.frame(lapply(x, function(y) {
        y<-gsub("SSC","TSS (°Brix)", y)
        y<-gsub("TA","TA (%)", y)
        y<-gsub("acidity","TA (%)", y)
        y<-gsub("TotalOA","Total OA (mg/g DW)", y)
        y<-gsub("Quinic","Quinic acid (mg/g DW)", y)
        y<-gsub("Malic","Malic acid (mg/g DW)", y)
        y<-gsub("Shikimic","Shikimic acid (mg/g DW)", y)
        y<-gsub("Citric","Citric acid (mg/g DW)", y)
        y<-gsub("TotalSugar","Total Sugar (mg/g DW)", y)
        y<-gsub("Sucrose","Sucrose (mg/g DW)", y)
        y<-gsub("Glucose","Glucose (mg/g DW)", y)
        y<-gsub("Fructose","Fructose (mg/g DW)", y)
      }))
    }

    return(x)
  }
}
