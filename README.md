<h1>Normal Transformation</h1>
The Normal Transformation project aims to analyze a dataset related to Parkinson's disease. It performs data quality checks, cleaning, exploratory analysis, and applies various transformation techniques. Additionally, it creates regression models to compare their performance using different evaluation metrics.

<h3>Features</h3>

1. Ensure data integrity by performing comprehensive quality checks and cleaning procedures. Handle missing values, outliers, and other inconsistencies present in the dataset. <br>

2. Exploratory Data Analysis: Gain insights into the dataset by exploring the joint distribution between multiple variables. Understand the relationships between different features and their impact on the target variable.<br>

3. Transformation Techniques: Utilize various transformation techniques such as Z-scaling, robust scaling, and logarithmic transformations to normalize and enhance the data for regression modeling.<br>

4. Regression Modeling: Create regression models using the transformed datasets. Capture the relationship between the input features and the target variable. Compare different models using a range of evaluation metrics.<br>

<h3>Dependencies</h3>

1. Base R functions: The code uses various built-in functions and operators provided by the base R language, such as read.csv, is.na, colSums, ifelse, length, which, summary, scale, median, IQR, log10, lm, etc.

2. Packages:
dlookr: The code uses functions from the dlookr package for outlier detection and diagnostics.
ggpubr and cowplot: These packages are used for data visualization, including creating box plots and scatter plots.
plotly: This package is used for 3D density plots.<br>

To run the code successfully, you need to have R installed on your system along with the required packages (dlookr, ggpubr, cowplot, and plotly). You can install these packages by running the code in the file. <br>

Please note that the code assumes the Parkinson's dataset is located at the specified file path. Please adjust the file path to match the actual location of your dataset.

<h3>Result</h3>
The project aims to provide the following outcomes:<br>
1. Cleaned and preprocessed dataset suitable for further analysis and modeling.<br>
2. Insights into the relationships and distributions of the variables through exploratory analysis.<br>
3. Normalized data using various transformation techniques for improved regression modeling.<br>
4. Regression models capturing the relationship between the input features and the target variable.<br>
5. Comparison of different regression models using a range of evaluation metrics to assess their performance.<br>
