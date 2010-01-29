(* TODO: dump symbolic and numerical data to separate files to
increase reliebility and performance *)

(* Mathematica Package *)

BeginPackage["StringIntegrator`"];
(* Exported symbols added here with SymbolName::usage *)


(* Begin["`Private`"] (\* Begin Private Context *\) *)

(* End[] (\* End Private Context *\) *)

(* $MaxExtraPrecision=1000; *)

Protect[x];

f[x_]:=x;

g[x_]:=x^2;

h[x_]:=E^x;

tlist={2.};
flist={g};
dataList=Flatten[Outer[List,flist,tlist],1];

(* WARNING! for T=2 and t=300. numerical error becomes significant (at
least for f(x)=x), thre reason is the numerical error for folding of
operator L (number representations in mathematic are of finite
precision and folding decreases accuracy of the coefficients in the
exponential rate, see Plots/Error). The numerical error for other T is
not known yet *)

tmin=0;tmax=284;dt=1;
testPoints=Range[tmin,tmax,dt];
testPointsRandom=NestList[#+Random[Real,{0.,1.}]&,tmin,Floor[2*tmax]];

nmax=Ceiling[tmax];

FunctionName[f_]:="f(x)="<>ToString[f[x]];

GenericDirName[]:="Results/";

FuncDirName[]:=GenericDirName[]<>"Func/";

DataDirName[]:=GenericDirName[]<>"Data/";

GenericFileName[f_,T_]:="f_"<>ToString[f[x],InputForm]<>"_T"<>ToString[T]<>"_";

FuncFileName[f_,T_]:=FuncDirName[]<>GenericFileName[f,T]<>".mx";

DataFileName[f_,T_]:=DataDirName[]<>GenericFileName[f,T]<>".dat";

gNorm[g_, T_] := Evaluate[g[#] - (g'[1] - g'[0] + T g[1])] &


DefineL[]:=Module[{},
		  Clear[L];
		  L[g_, T_, g0_] := Module[{returnVal,f},

					   (* Print["Input accuracy = ",Accuracy[g],"   ",Accuracy[T]]; *)
					   Print["Folding..."];

					   Print["Done in ",
						 Timing[returnVal=
							Evaluate[
							    (* Simplify[ *)
								g[#] - Exp[-# T] Integrate[Exp[s T] g[s], {s, 0, #}] +
								Exp[-# T] (g[1] - g[0])] &][[1]]];

					   Print["Accuract = ",Accuracy[returnVal],"\n=======\n"];

					   DefAppend[FuncFileName[g0,T],L[g,T,g0],returnVal]
					  ]]

LFold[g_,T_, n_] := LFold[g, T, n] =
    Module[{p, k, pt},
	   Print["LFold[",g,",",T,",",n,"]"];
	   k = 1;			(*Fold counter *)
	   p = Timing[Evaluate[
	       NestList[
		   (
		       Print[	(*Print iteration number *)
			      "Iterating... n = ", k++];
		       Print[	(*Print some usefuls status data *)
			      "   T = ", T,
			      " | f(x) = ", gNorm[g,T][x],
			      " | DumpFile = ", FuncFileName[g,T]
			    ];
		       L[#, T, g]	(*Fold the operator *)
		   ) &,
		   gNorm[g, T],	(*Starting point of the folding *)
		   n]]];	(*Fold n-times *)

	   Print["Done in ", p[[1]], " seconds"];

	   Function[t,Piecewise[
	       {p[[2, #]][t - (# - 1)],
		(# - 1) <= t < #} & /@ Range[n + 1]]]]

DefineTPV[]:=Module[{},
		    Clear[TestPointValue];
		    TestPointValue[g_,T_,t_]:=Module[{returnVal},
						     (* save after calculating whole
							grid*)
						     Print["Summing for t = ",N[t]];
						     returnVal=N[LFold[g,T,nmax][t],{Infinity,20}];
						     Print["Result = ",returnVal,
							   " +/- ",10^-Accuracy[returnVal]/Abs[returnVal],
							   "% (Accuracy = ",Accuracy[returnVal],")"];
						     func=Evaluate[g[#]]&; (*use the full function form of g *)
						     (* DefAppend[FuncFileName[g,T],TestPointValue[g,T,t],returnVal] *)
						     returnVal
						    ]] (*TODO:
							 Global variable nmax used inside
							 function, this is evil! *)

TestPointExportValues[g_,T_,tlist_]:=Module[{file,t},
					    Print["Dumping test point values to ", DataFileName[g,T]];
					    Export[DataFileName[g,T],
						   {#,
						    t=TestPointValue[g,T,#],
						    Log[t^2],
						    Log[t^2]/EnergyT0[gNorm[g,T]],
						    T}&
						   /@tlist,
						   "Data"];
					    (* file=OpenWrite[DataFileName[g,T]]; *)
					    (* WriteString[file, #, "   ", *)
					    (* 		t=TestPointValue[g,T,#], "   ", *)
					    (* 		Log[t^2],"   ", *)
					    (* 		Log[t^2/EnergyT0[g,T]],"\n"]&/@tlist; *)
					    (* Close[file]; *)
					    Print["Dumping done"];
					   ]

EnergyT0[g_,T_]:=EnergyT0[g,T]=		(*This formulae dosen't seem to work at all *)
    Integrate[3/2*g'[s]^2,{s,0,1}]+
    (g'[1]-g'[0])^2-
    Integrate[g'[s]*g'[1/2-s],{s,1/2,1}]

SetAttributes[DefAppend,HoldRest];

DefAppend[file_,s_,df_]:=Module[{str,f},
			    f=OpenAppend[file];
			    WriteString[f,ToString[Unevaluated[s],InputForm]<>"="<>ToString[df,InputForm]<>"\n\n"];
			    Close[f];
			    s=df
			   ]

(* Clears the definitions of L and TestPointValue, and calculates
their new values using input from apropriate file*)
FillUp[f_,T_,nmax_]:=Module[{g=Evaluate[f[#]]&},
			    DefineL[];
			    DefineTPV[];
			    Print["Reading file ",FuncFileName[g,T]];
			    Get[FuncFileName[g,T]];
			    LFold[g,T,nmax];
			    TestPointExportValues[g,T,testPointsRandom];
			   ]

Repair[g_,T_,nmax_]:=Module[{},
			    Print["Repairing file ",FuncFileName[g,T]];
			    DefineL[];
			    DefineTPV[];
			    Get[FuncFileName[g,T]];
			    DeleteFile[FuncFileName[g,T]];
			    Save[FuncFileName[g,T],"StringIterator`"];
			   ]


EndPackage[]

CreateDirectory[{GenericDirName[],DataDirName[],FuncDirName[]}];

Off[General::noopen];

Apply[FillUp[#,#2,nmax]&,dataList,{1}];
(* Table[Apply[FillUp[#,#2,n]&,dataList,{1}],{n,0,nmax,30}]; *)

Run["./plotter.sh"];

Quit[];
