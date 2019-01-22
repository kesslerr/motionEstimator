# motionEstimator
estimates scan to scan motion of fMRI data by use of realignemt parameters (6 parameter rigid body)

how to use:

1. realign all your subjects. If you use SPM, you will get a file named "rp_XXX.txt"
2. put all rp files in the folder called "data" in your current working directory
3. run script

what it does:
- it translates the "rotational parameters" (pitch, roll, yaw, in columns 4,5&6 of your rp file) into a translational parameter
- therefore, it uses an average cortical distance of 65mm (see Wilke et al for that value)
- then it adds the rotational parameters to the already existend translational parameters (x,y,z, in columns 1,2&3 of your rp file) 
- it calculates the location differences between two scans
    e.g. it uses the x, y, and z coordinates and uses the (extended) Pythagorean formula to calculate the absolute motion
- it plots the scan to scan differences (number of data points is one less then the number of scans acquired, if you want to use it as covariate in a statistical model, you need to add another time point, e.g. 0 at the beginning)
- it plots the distribution of motion of each subjects, and draws boxplots for different subjects
- with this, it is easy to detect subjects that show a uncommon motion profile in your subjects
