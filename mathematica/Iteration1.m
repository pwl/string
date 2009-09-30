Protect[x];

f[x_]:=x
g[x_]:=x^2
h[x_]:=E^x

tlist={1.,2.};
flist={f,g,h};
dataList=Outer[List,flist,tlist];

tmin=10.;tmax=20.;dt=1.;
testPoints=Range[tmin,tmax,dt];

nmax=Ceiling[tmax]-1;

FunctionName[f_]:="f(x)="<>ToString[f[x]]

GenericDirName[]:="Results"

GenericFileName[f_,T_]:="f_"<>ToString[f[x],InputForm]<>"_T"<>ToString[T]

FuncFileName[f_,T_]:=GenericFileName[f,T]<>".mx"
DataFileName[f_,t_]:=GenericFileName[f,T]<>".dat"

gNorm[g_, T_] := Evaluate[g[#] - (g'[1] - g'[0] + T g[1])] &

L[g_, T_] := L[g, T] =
    Module[{pt},
	   (*PrintTemporary["Iterating..."];*)
	   pt=PrintTemporary["Saving to file ",FuncFileName[g,T]];
	   DumpSave[FuncFileName[g,T], L]; (*Save previous definitions of L *)
	   NotebookDelete[pt];
	   Evaluate[
	       Simplify[
		   g[#] - Exp[-# T] Integrate[Exp[s T] g[s], {s, 0, #}] +
		   Exp[-# T] (g[1] - g[0])]] &]

LFold[g_, t_, n_, T_] := LFold[g, t, n, T] =
    Module[{p, k, pt},
	   Get[FuncFileName[g,T]];	(*Get dumped save *)
	   k = 1;			(*Fold counter *)
	   p = Timing[Evaluate[
	       NestList[
		   (
		       NotebookDelete[pt];
		       pt=PrintTemporary[ (*Print status *)
					  "Iterating... n = ", k++,
					  " ** T = ", T,
					  " ** f(x) = ", gNorm[g,T][x],
					  " ** DumpFile = ", FuncFileName[g,T]
					];
		       L[#, T]	(*Fold the operator *)
		   ) &,
		   gNorm[g, T],	(*Starting point of the folding *)
		   n]]];	(*Fold n-times *)

	   Print["Done in ", p[[1]], " seconds"];

	   Piecewise[
	       {p[[2, #]][t - (# - 1)],
		(# - 1) <= t < #} & /@ Range[n + 1]]];

TestPointValue[g_,T_,t_]:=Module[{pt},
				 DumpSave[FuncFileName[g,T]]; (*Lasts
				 long, not that necessary here, better
				 save after calculating whole grid*)
				 TestPointValue[g,t,T]=LFold[g,t,Ceiling[t]-1,T]]

TestPointCreateValues[g_,T_,tlist_]:=Module[{pt},
					    (
						NotebookDelete[pt];
						pt=PrintTemporary["Summing for t = ",#];
						TestPointValues[g,T,#]
					    )&/@tlist;
					   DumpSave[FuncFileName[g,T],TestPointValue];
					   ]

TestPointExportValues[g_,T_,tlist_]:=Module[{file},
					    TestPointCreateValues[g,T,tlist];
					    file=OpenWrite[DataFileName[g,T]];
					    Write[file,#,"   ",TestPointValue[g,T,#]]&/@tlist;
					    Close[file];
					   ]

CreateDirectory[GenericDirName[]];
SetDirectory[GenericDirName[]];


Get[FuncFileName[#1,#2]]&@@@dataList; (*get the results already calculated *)

LFold[#1, t, nmax, #2]&@@@dataList; (*Fold L for all the data *)
