#include <R.h>             //specific R library
#include <Rdefines.h>      //specific R library
#include <Rmath.h>

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Applic.h>

#include <queue>
//#include <vector>
//#include <fstream>
//#include <algorithm>
//#include <math.h>
//#include <iostream>b
//#include <time.h>
//#define n 10000
//#define n1 5000
//#define n2 5000
//#define d 10
//#define k 11
//#include "ClosestPoints.h"

using namespace std;

typedef double Elem;

typedef int Index;

//typedef int Count;


struct DIPair
{
	Elem e;
	Index i;
	DIPair() : e(0), i(0) {}
	DIPair(const DIPair& di0) : e(di0.e), i(di0.i) {}
	DIPair(Elem e0, Index i0) : e(e0), i(i0) {}
	inline bool operator< (const DIPair& p2) const { return (e < p2.e); }
	~DIPair() {}
};


//typedef vector<Elem> OnePoint;

//typedef vector< OnePoint * > Points;

typedef priority_queue<DIPair> OneClosestPoints;

//typedef vector< OneClosestPoints * > ClosestPoints;

//typedef vector<Count> Counts;

//int n=100, n1=50, n2=50,d=10,k=11;


extern "C"{

Elem dist(Elem * v1, Elem * v2, Index d)
{
	Elem sum=0;
	for (int i=0; i!=d; ++i)
	{
		sum+=( v1[i] - v2[i] ) * ( v1[i] - v2[i] );
	}
	//	return sqrt(sum);
	return sum;
}

void knn(double * points, int * n, int * d, int * k, int * counts)
{
	int * closest = new int[n[0]*k[0]];
	for (int i=0; i!=n[0]; ++i)
	{
		OneClosestPoints * q = new OneClosestPoints;
		for (int j=0; j!=n[0]; ++j)
		{
			if (i!=j)
			{
				DIPair dis = DIPair ( dist ( points+i*(d[0]+1), points+j*(d[0]+1), d[0] ) , j );
				if ( q->size() == k[0] )
				{
					if (dis.e < q->top().e)
					{
						q->push(dis);
						q->pop();
					}
				}
				else
				{
					q->push(dis);
				}
			}
		}
		for (int j=0; j!=k[0]; ++j)
		{
			closest[i*k[0]+j] = q->top().i;
			q->pop();
		}
		delete q;
	}
	
	for (int i=0; i!=n[0]; ++i)
	{
		for (int j=0; j!=k[0]; ++j)
		{
			if ( points[closest[i*k[0]+j]*(d[0]+1)+d[0]] == points[i*(d[0]+1)+d[0]] )
			{
				counts[i]+=1;
			}
		}
	}
	delete [] closest;
	// time_t t0 = time(0);
	//Elem s[n][d];
	//input(s);
	//cout << "Time = " << time(0) - t0 << " sec \n";
	//ClosestPoints * cl = closest(s);
	//cout << "Time = " << time(0) - t0 << " sec \n";
	//Counts * ct = count(cl);
	//cout << "Time = " << time(0) - t0 << " sec \n";
	//delete cl;
	//output(ct);
	//delete ct;
	//cout << "Time = " << time(0) - t0 << " sec \n";
	//int pp;
	//cin >> pp;
	
}

}
