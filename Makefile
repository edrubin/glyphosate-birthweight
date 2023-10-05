# Step 5: Figures and tables 

# Step 4: Analysis

# Step 3: Combine data

# Step 2: Train Water ML model 

# Step 1: Define treatment 

# Step 0: Prep the data 

# County area 
data/pop-area-empl/cnty-area-dt.fst, data/pop-area-empl/cnty-pop-90.fst, data/pop-area-empl/cnty-pop-00.fst, data/pop-area-empl/cnty-pop-10.fst, data/pop-area-empl/cnty-pop-dt.fst: R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript R/00-data-prep/controls/00-cnty-pop-area.R

# Education data
data/pop-area-empl/edu-dt.fst: R/00-data-prep/controls/03-education.R data/pop-area-empl/Education.xls
	Rscript R/00-data-prep/controls/03-education.R	