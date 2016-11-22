## Macro algae project

This is the git repository for the `makroalgaeindicators` R package.

```.sas

/* This part is for the GLMM */

%macro cumcov_ind(area,period,no_years);
data covparms_cumcover_in;
   set covparms_cumcover;
   if CovParm='year(vandomr*period)' then estimate=estimate*(1-&no_years/6);
run;
proc MIXED data=makalge_trend_modif(where=(vandomraade=&area and period=&period)) covtest method=ml;
   class vandomraade station year month proevetager interval period;
   model log_cumcover_mod=vandomraade*period / solution noint covb;
   random station(vandomraade*period) year(vandomraade*period) station*year(vandomraade*period) proevetager;
   parms / pdata=covparms_cumcover_in hold=(1,2,3,4,5);
   ODS output SolutionF=parmest_cumcover_test covB=covB_cumcover_test;
run;
quit;
%mend cumcov_ind;
%cumcov_ind('Als Fjord','2001-2006',6);
%cumcov_ind('Als Fjord','2007-2012',2);
%cumcov_ind('Als Fjord','2013-2016',3);

/* Producing parameter vector for calculating indicator */

data parmest_cumcover_test;
   set parmest_cumcover_test;
   call symput('parm_assessunit',estimate);
run;
data parmest_cumcover_in;
   set parmest_cumcover_test parmest_cumcover(where=(vandomraade=''));
run;

/* Producing covariance matrix for calculating indicator 

Jacob: I will need some explanation for the rest of the code. I do not understand few things:

1. the 1:174 loop (used to extract relevant parameters 167-174)
2. how you get a value for _N_
3. 

*/

data covB_cumcover_test;
   set covB_cumcover_test;
   call symput('cov_assessunit',col1);
run;
data covB_cumcover_in;
   array col{1:174} col1-col174;
   set makrolib.covB_cumcover;
   if _N_=1 then do; col1=&cov_assessunit; do i=167 to 174; col[i]=0; end; output; end;
   col1=0;
   if vandomraade='' then output;
   drop vandomraade month period col2-col166 row effect i;
run;

/* At this point we have the vector and the matrix */

%let st_depth=10; /* should be defined by the user, this is the std_depth variable in R*/
proc iml;
   use parmest_cumcover_in; /* vector with the 9 elements */
   read all var{estimate} into beta_vector; 
   use covB_cumcover_in; /* 9 x 9 matrix */
   read all into V_beta_matrix;
   L_vector= {1 0.2 0.2 0.2 0.2 0.2 &st_depth 50 0};
   
   /* 1 row and 2 columns (just to create an empty matirx) */
   create lsmean_cumcover var{estimate variance};
   
   
   estimate=L_vector*beta_vector;
   variance=L_vector*V_beta_matrix*L_vector`; append; /* do not forget ' which is transpose */

quit;

/* 5 diffrent classes */
%let boundary_HG=74.2;
%let boundary_GM=40.8;
%let boundary_MP=20.4;
%let boundary_PB=13.4;

/* 10000 = n_iter */
data lsmean_cumcover;
   set lsmean_cumcover;
   format status $10.;
   do i=1 to 10000;
      cumcover=exp(estimate+rannor(-1)*sqrt(variance))+1;
             status='High';
             if cumcover<&boundary_HG then status='Good';
             if cumcover<&boundary_GM then status='Moderate';
             if cumcover<&boundary_MP then status='Poor';
             if cumcover<&boundary_PB then status='Bad';
      output;
   end;
run; /* at the end we have a vector of n = 10000 */


proc univariate data=lsmean_cumcover noprint;
   var cumcover;
   output out=cumcover_dist median=cumcover_median mean=cumcover_mean pctlpre=P_ pctlpts=0.5, 1, 2.5, 5, 50, 95, 97.5, 99, 99.5;
run;
```