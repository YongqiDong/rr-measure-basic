The Anaconda Python JupyterLab should be used for running the notebook.
The following preparetions before the running the FGBR_OC.ipynb notebook should be performed.

A) Required preparetions for the FGBR_OC.ipynb file:

Step 1. The required my_load_dataset function should be replaced in the FGBR_OC.ipynb file (line 43).
Step 2. The required shape parameters should be adjusted in the FGBR_OC.ipynb file (lines 54-55).
Step 3. In case of the WABL method, the cycle of the optimism parameter (c) shoul be activated (lines 192-193), 
	and lines 197-198 should be commented. 
	In case of the other defuzzification methods, the cycle of the optimism parameter (c) shoul be commented (lines 192-193), 
	and lines 197-198 should be activated.

B) Required preparetions for the fuzzy_operations.py file:

Step 4. Only the required defuzzification method should be uncommented in the defuz() function (lines 8-10),  
	and the file must be saved.
	
	
After the preparations above, the FGBR_OC.ipynb notebook should be run.
