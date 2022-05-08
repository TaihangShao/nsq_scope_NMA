clear all
version 15
set more off

*Set working directory and load data
cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check/"
import delimited RP_PFS.csv, clear
drop i

* Set the colour scheme
set scheme s1color

*Declare survival data
stset time, fail(event=1)

* Pairiwse meta-analysis
preserve
	keep if studycode==4 | studycode==5
	keep if txcode==1 | txcode==5
	
	gen trtc=0 if txcode==1
	replace trtc=1 if txcode==5
	
	ipdmetan, study(study) eform effect("HR") forestplot(xlabel(0.4(0.2)1.4)): stcox i.trtc
restore	
