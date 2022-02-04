#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstring>
#include <vector>
#define _USE_MATH_DEFINES
#include <math.h>
#include <complex>
#include <functional>
#include <algorithm> 
#include <numeric>
#include <assert.h>
#include <limits>

using namespace std;


void find_2_nn_dist(vector<double>& D1,vector<double>& D2, vector<double>& X, int N, int ncoords, bool periodicB)
{
    /*
	Class definition for finding the distances to the first two nearest neighbors of each point
	input: 
		X: Data
		N: Number of points in the data
		ncoords: number of coordinates of the data
		periodicB: bool for periodic boundary conditions

	output:
		D1: vector of distance to the first nearest neighbor for each data point
		D2: vector of distance to the sencond nearest neighbor for each data point
    */

	double maxdist=numeric_limits<double>::max();;
	double L[ncoords];
	double dist, d1, d2;
	for(int cc=0; cc<ncoords; cc++){
	    	L[cc]=1.;  
	}
	double Xtemp[ncoords];
	//iteration over each pair of points xi,xj
	for(int i=0; i<N; i++){
		d1=maxdist;
		d2=maxdist;
		for(int j=0; j<N; j++){
			if(j!=i){
				dist=0.;
				// iterating over each coordinate of the pair of points xi[coord], xj[coord]
				for(int cc=0; cc<ncoords; cc++){
					Xtemp[cc]=X.at(i*ncoords+cc)-X.at(j*ncoords+cc);
					//periodic boundary condition
					if(periodicB){
						if(abs(Xtemp[cc])>L[cc]*0.5) 
							if(X.at(i*ncoords+cc)>X.at(j*ncoords+cc)) Xtemp[cc]=L[cc]-Xtemp[cc];
							else Xtemp[cc]=L[cc]+Xtemp[cc];
					}
					dist+=Xtemp[cc]*Xtemp[cc];
				}
				dist=sqrt(dist); 
				// saving lower distance to d1 and d2
				if(dist<d1&&dist<d2){
					d2=d1;
					d1=dist;
				}
				else if(dist>=d1&&dist<d2){
				d2=dist;
				}                   
			}
		}
		D1.push_back(d1);
		D2.push_back(d2);
	}

	return;
}


void compute_ID(vector<double>& D1, vector<double>& D2, double& dim, int N, double frac, int nbox, int& N_retained)
{
	/*
    Class definition for TWO-NN: Intrinsic dimension estimator by a minimal neighborhood information.
    The TWO-NN estimator uses only the distances to the first two nearest neighbors of each point.
	input:
		D1: vector of distance to the first nearest neighbor for each data point
		D2: vector of distance to the sencond nearest neighbor for each data point
		N: Number of points in the data
		frac: fraction of retained data for better performance in ID computation
		periodicB: bool for periodic boundary conditions
		N_retained: Number of points retained by frac

	output:
		dim: Intrinsic Dimension (ID) of the data obtainde by the TWO-NN algorithm

	References
    ----------
    E. Facco, M. d’Errico, A. Rodriguez and A. Laio, Estimating the intrinsic dimension of datasets by a minimal neighborhood information. `Sci. reports` 7, 12140 (2017) 
	*/

	vector<double> NU;

	double num, den;
	double nu;

	for(int i=0; i<N; i++)
	{ // computing the ratio of distances for each point
		num=D2[i];
		den=D1[i];

		nu=num/den;
		
		NU.push_back(nu);
	
		if(nu==1.) cout<<"Point "<<i<<" has the first two neighbors at the same distance!"<<endl;

	}

	sort(NU.begin(), NU.end());

	// Fit
	double XX[N], YY[N]; //XX=log(nu), YY=-log(1-F(nu))

  	// only for the full dataset write the relevant files
	if (nbox==1) 
	{	
		//file containing the list of distances between the point and its first and second neighbor
		ofstream file_rlist("r_list.dat");
		//file containing the list of nu values
		ofstream file_nulist("nu_list.dat");   
		//file containing the coordinates to plot the S-set
		ofstream file_fun("fun.dat"); 

		for(int i=0; i<N; i++)
		{	//XX=log(nu)
	 		XX[i]=log(NU.at(i));
			//YY=-log(1-F(nu))
			YY[i]=-log(1.-double(i)/double(N));

			file_fun<<XX[i]<<' '<<YY[i]<<endl;

			num=D2[i];
			den=D1[i];

			nu=num/den;
			
			file_rlist<<den<<' '<<num<<endl;
			file_nulist<<nu<<endl;
		}

		file_fun.close();
		file_nulist.close();
		file_rlist.close();

	}
  
	for(int i=0; i<N; i++)
	{
	 	XX[i]=log(NU.at(i));
		YY[i]=log(1.-double(i)/double(N));
	}

	double sumX, sumY, sumXY, sumX2, sumY2, sumErr;

	sumX=0.;
	sumY=0.;
	sumXY=0.;
	sumX2=0.;
	sumY2=0.;
	sumErr=0.;
 

	int Ncut=int(double(N)*frac);

	if (nbox==1)
	{
		N_retained=Ncut;
	}

	for(int i=0; i<Ncut; i++)
	{
		sumX+=XX[i];
		sumY+=YY[i];
		sumXY+=XX[i]*YY[i];
		sumX2+=XX[i]*XX[i]; 
		sumY2+=YY[i]*YY[i];     
	}

	//fit formula with the straight line through the origin a*x
	dim=-sumXY/sumX2;  

	double minval=sqrt(sumY2-sumXY*sumXY/sumX2)/double(Ncut);

	if (nbox==1) cout<< "estimated dimension= "<<dim<<' '<<endl;

}

