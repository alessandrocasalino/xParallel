(* ::Package:: *)

(* ::Title:: *)
(*xParallel*)


(* ::Subsection:: *)
(*Parallelization for xAct*)


(* ::Affiliation:: *)
(*Alessandro Casalino, alessandro.casalino@unitn.it*)


(* ::Text:: *)
(*(c) 2020, under GPL*)


(* ::Text:: *)
(*This package introduces simple parallelization functions to xAct.*)
(*   *)
(*The package xParallel is distributed under the GNU GPL License, and runs on top of xTensor, a free package for fast manipulation of abstract tensor expressions in Mathematica.*)


(* ::Input:: *)
(*DateList[]*)


(* ::Input::Initialization:: *)
xAct`xParallel`$Version={"0.0.1",{2020,9,16}};


(* ::Input::Initialization:: *)
xAct`xParallel`$xTensorVersionExpected={"1.1.3",{2018,2,28}};


(* ::Chapter:: *)
(*1. Initialization*)


(* ::Subsection::Closed:: *)
(*1.1 GPL*)


(* ::Input::Initialization:: *)
(* xParallel, parallelization for xAct *)

(* Copyright (C) 2020 Alessandro Casalino *)

(* This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of
 the License,or (at your option) any later version.

This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307,
  USA. 
*)


(* ::Subsection::Closed:: *)
(*1.2 Info package*)


(* :Title: xParallel *)

(* :Author: Alessandro Casalino *)

(* :Summary: Parallelization for xAct *)

(* :Brief Discussion:
   - Configuration of kernels
   - Definitions on all created kernels
   - Some parallelized functions already available 
*)
  
(* :Context: xAct`xParallel` *)

(* :Package Version: 0.0.1 *)

(* :Copyright: Alessandro Casalino (2020) *)

(* :History: *)

(* :Keywords: *)

(* :Source: xParallel.nb *)

(* :Warning: The package is still a work in progress. *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations:
	- Not all functions has been implemented
	- Some algorithm might be slower than non-parallel ones in some cases
*)


(* ::Subsection:: *)
(*1.3 Begin Package*)


(* ::Text:: *)
(*Protect against multiple loading of the package:*)


(* ::Input::Initialization:: *)
With[{xAct`xParallel`Private`xPertSymbols=DeleteCases[Join[Names["xAct`xParallel`*"],Names["xAct`xParallel`Private`*"]],"$Version"|"xAct`xParallel`$Version"|"$xTensorVersionExpected"|"xAct`xParallel`$xTensorVersionExpected"]},
Unprotect/@xAct`xParallel`Private`xPertSymbols;
Clear/@xAct`xParallel`Private`xPertSymbols;
]


(* ::Text:: *)
(*Decide which is the last package being read:*)


(* ::Input::Initialization:: *)
If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xParallel`"];


(* ::Input:: *)
(*xAct`xCore`Private`$LastPackage*)


(* ::Text:: *)
(*Explicit (not hidden) import of other packages. QUESTION: Do we really need ExpressionManipulation?*)


(* ::Input::Initialization:: *)
BeginPackage["xAct`xParallel`",{"xAct`xTensor`","xAct`xPerm`","xAct`xCore`","xAct`xPert`","xAct`xCoba`","xAct`ExpressionManipulation`"}]


(* ::Text:: *)
(*Check version of xTensor. We simply compare dates:*)


(* ::Input::Initialization:: *)
If[Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],Throw@Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected]]


(* ::Text:: *)
(*Welcome message:*)


(* ::Input::Initialization:: *)
Print[xAct`xCore`Private`bars];
Print["Package xAct`xParallel`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2020, Alessandro Casalino, under the General Public License."];


(* ::Text:: *)
(*We specify the context xAct`xPert` to avoid overriding the Disclaimer of the other packages. However we need to turn off the message General:shdw temporarily:*)


(* ::Input::Initialization:: *)
Off[General::shdw]
xAct`xParallel`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


(* ::Text:: *)
(*If this is the last package show the GPL short disclaimer:*)


(* ::Input::Initialization:: *)
If[xAct`xCore`Private`$LastPackage==="xAct`xParallel`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


(* ::Text:: *)
(*Note that symbols in the Global` context cannot be accessed while building the package:*)


(* ::Input:: *)
(*$ContextPath*)


(* ::Input:: *)
(*$Context*)


(* ::Subsection:: *)
(*1.4 Usage messages*)


(* ::Text:: *)
(*Usage messages for xParallel functions*)


(* ::Text:: *)
(*Initial definitions*)


(* ::Input::Initialization:: *)
$ParallelDefInfoQ::usage=
"$ParallelDefInfoQ is a boolen value controlling the messages printed by xParallel functions. If is set to False, only the first kernel will generate a message as usual.";
ParallelConfigure::usage=
"ParallelConfigure[N] will launch N kernels. If N > cores of your system or kernels of your Mathematica licence, the number will be the minimum between these two. The same if N is not specified. Note that $DistributedContexts will be set to None.";
ParallelClean::usage =
"ParallelClean[] will clear all kernels allocated by xParallel.";


(* ::Text:: *)
(*xTensor*)


(* ::Input::Initialization:: *)
ParallelDefManifold::usage =
"ParallelDefManifold works like DefManifold, but allocating the definitions on all kernels.";
ParallelDefMetric::usage =
"ParallelDefMetric works like DefMetric, but allocating the definitions on all kernels.";
ParallelDefTensor::usage =
"ParallelDefTensor works like DefTensor, but allocating the definitions on all kernels.";
ParallelDefScalarFunction::usage =
"ParallelDefScalarFunction works like DefScalarFunction, but allocating the definitions on all kernels.";
ParallelDefConstantSymbol::usage =
"ParallelDefDefConstantSymbol works like DefConstantSymbol, but allocating the definitions on all kernels.";

ParallelContractMetric::usage =
"ParallelContractMetric works like ContractMetric, but performing the computation on all available kernels.";
ParallelSortCovDs::usage =
"ParallelSortCovDs works like SortCovDs, but performing the computation on all available kernels.";
ParallelSimplification::usage =
"ParallelSimplification splits the equation in a number of parts given by the allocated kernels. It then performs the Simplification on each of these parts. The result will not be the same as making a Simplification on the original function, therefore it might be necessary to make a ParallelSimplification and then another Simplification on the resulting equation. Although this is in general faster than a simple Simplification, in some cases it might be the opposite.";
ParallelToCanonical::usage =
"ParallelToCanonical splits the equation in a number of parts given by the allocated kernels. It then performs the ToCanonical on each of these parts.";


(* ::Text:: *)
(*xPert*)


(* ::Input::Initialization:: *)
ParallelDefMetricPerturbation::usage =
"ParallelDefMetricPerturbation works like DefMetricPerturbation, but allocating the definitions on all kernels.";
ParallelDefTensorPerturbation::usage =
"ParallelDefTensorPerturbation works like DefTensorPerturbation, but allocating the definitions on all kernels.";

ParallelPerturbation::usage =
"ParallelPerturbation works like Perturbation, but performing the computation on all available kernels.";
ParallelExpandPerturbation::usage =
"ParallelExpandPerturbation works like ExpandPerturbation, but performing the computation on all available kernels.";
ParallelVarD::usage =
"ParallelVarD works like VarD, but performing the computation on all available kernels.";


(* ::Text:: *)
(*xCoba*)


(* ::Input::Initialization:: *)
ParallelDefChart::usage =
"ParallelDefChart works like DefChart, but allocating the definitions on all kernels.";
ParallelMetricInBasis::usage =
"ParallelMetricInBasis works like MetricInBasis, but allocating the definitions on all kernels.";
ParallelMetricCompute::usage =
"ParallelDefMetricCompute works like DefMetricCompute, but allocating the definitions on all kernels.";
ParallelComponentValue::usage =
"ParallelComponentValue works like ComponentValue, but allocating the definitions on all kernels.";

ParallelToBasis::usage=
"ParallelToBasis works like ToBasis, but performing the computation on all available kernels.";
ParallelComponentArray::usage=
"ParallelComponentArray works like ComponentArray, but performing the computation on all available kernels.";
ParallelTraceBasisDummy::usage=
"ParallelTraceBasisDummy works like BasisDummy, but performing the computation on all available kernels.";
ParallelToValues::usage=
"ParallelToValues works like ToValues, but performing the computation on all available kernels.";

ParallelInCoordinates::usage=
"ParallelInCoordinates[ch][equation] evaluates the input equation term by term with ToBasis[ch], ToValues, TraceBasisDummy and ToValues again, in parallel.";


(* ::Subsection::Closed:: *)
(*1.5 Begin Private*)


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Input:: *)
(*$Context*)


(* ::Input:: *)
(*$ContextPath*)


(* ::Subsection:: *)
(*1.5 Initial definitions*)


(* ::Text:: *)
(*Verbose flag for definitions*)


(* ::Input::Initialization:: *)
$ParallelDefInfoQ =False;


(* ::Text:: *)
(*Function to configure the kernels*)


(* ::Input::Initialization:: *)
ParallelConfigure[N_:Min[$ProcessorCount,$MaxLicenseSubprocesses]]:=(LaunchKernels[Min[N,$ProcessorCount,$MaxLicenseSubprocesses]];
Print["** ParallelConfigure: Maximum number of kernels allocable is ", Min[$ProcessorCount,$MaxLicenseSubprocesses]];
Print["** ParallelConfigure: Configuration with ", Length[Kernels[]]," kernels"];
$DistributedContexts=None;
ParallelNeeds["xAct`xPert`"];)


(* ::Text:: *)
(*Function to shut down the kernels*)


(* ::Input::Initialization:: *)
ParallelClean[]:=(CloseKernels[];
Print["** ParallelConfigure: Parallel kernels has been shutted down"];)


(* ::Chapter:: *)
(*2. Parallel definitions*)


(* ::Subsection:: *)
(*2.1 xTensor*)


(* ::Input::Initialization:: *)
ParallelDefManifold[M_,dim_,indices_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefManifold: Parallelization definition"]];DefManifold[M,dim,indices,FilterRules[{options},Options[DefManifold]]];If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];ParallelEvaluate[DefManifold[M,dim,indices,FilterRules[{options},Options[DefManifold]]],Kernels[]];If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)

ParallelDefMetric[signdet_,metric_[a_,b_],covd_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefMetric: Parallelization definition"]];
DefMetric[signdet,metric[a,b],covd,FilterRules[{options},Options[DefTensor]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[DefMetric[signdet,metric[a,b],covd,FilterRules[{options},Options[DefTensor]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)

ParallelDefTensor[tensor_,dependencies_,options___?OptionQ]:=ParallelDefTensor[tensor,dependencies,GenSet[],options];
ParallelDefTensor[tensor_,dependencies_,sym_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefTensor: Parallelization definition"]];
DefTensor[tensor,dependencies,sym,FilterRules[{options},Options[DefTensor]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];ParallelEvaluate[DefTensor[tensor,dependencies,sym,FilterRules[{options},Options[DefTensor]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];)

ParallelDefScalarFunction[f_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefScalarFunction: Parallelization definition"]];
DefScalarFunction[f,FilterRules[{options},Options[DefScalarFunction]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];ParallelEvaluate[DefScalarFunction[f,FilterRules[{options},Options[DefScalarFunction]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];)

ParallelDefConstantSymbol[symbol_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefScalarFunction: Parallelization definition"]];
DefConstantSymbol[symbol,FilterRules[{options},Options[DefConstantSymbol]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];ParallelEvaluate[DefConstantSymbol[symbol,FilterRules[{options},Options[DefConstantSymbol]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];)


(* ::Subsection:: *)
(*2.2 xPert*)


(* ::Input::Initialization:: *)
ParallelDefMetricPerturbation[metric_,metpert_,param_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefMetricPerturbation: Parallelization definition"]];
DefMetricPerturbation[metric,metpert,param,FilterRules[{options},Options[DefMetricPerturbation]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[DefMetricPerturbation[metric,metpert,param,FilterRules[{options},Options[DefMetricPerturbation]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)

ParallelDefTensorPerturbation[pert_,tensor_,args_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** DefTensorPerturbation: Parallelization definition"]];
DefTensorPerturbation[pert,tensor,args,FilterRules[{options},Options[DefTensorPerturbation]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[DefTensorPerturbation[pert,tensor,args,FilterRules[{options},Options[DefTensorPerturbation]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)


(* ::Subsection:: *)
(*2.3 xCoba*)


(* ::Input::Initialization:: *)
ParallelDefChart[basis_,args_,indices_,coords_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** ParallelDefChart: Parallelization definition"]];
DefChart[basis,args,indices,coords,FilterRules[{options},Options[DefChart]]];
(*If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[DefChart[basis,args,indices,coords,FilterRules[{options},Options[DefChart]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];*))

ParallelMetricInBasis[metric_,basis_,matrix_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** ParallelDefMetricInBasis: Parallelization definition"]];
MetricInBasis[metric,basis,matrix,FilterRules[{options},Options[MetricInBasis]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[MetricInBasis[metric,basis,matrix,FilterRules[{options},Options[MetricInBasis]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)

ParallelMetricCompute[g_,ch_,T_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** ParallelMetricCompute: Parallelization definition"]];
MetricCompute[g,ch,T,FilterRules[{options},Options[MetricCompute]]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[MetricCompute[g,ch,T,FilterRules[{options},Options[MetricCompute]]],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)

ParallelComponentValue[expr_,value_,options:OptionsPattern[]]:=(
If[$ParallelDefInfoQ ==True,Print["** ParallelComponentValue: Parallelization definition"]];
ComponentValue[expr,value];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=False,Kernels[]]];
ParallelEvaluate[ComponentValue[expr,value],Kernels[]];
If[$ParallelDefInfoQ ==False,ParallelEvaluate[$DefInfoQ=True,Kernels[]]];)


(* ::Chapter:: *)
(*3. Parallel functions*)


(* ::Subsection:: *)
(*3.1 xTensor*)


(* ::Input::Initialization:: *)
ParallelContractMetric[equation_] := Module[{},Return[Plus@@ParallelMap[ContractMetric[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelSortCovDs[equation_] := Module[{},Return[Plus@@ParallelMap[SortCovDs[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelSimplification[equation_]:=Module[{},Plus@@ParallelMap[Simplification[Plus@@#]&,Partition[Quiet[AppendTo[List@@(equation//Expand),0]],Length[Kernels[]]]]]

ParallelToCanonical[equation_]:=Module[{},Plus@@ParallelMap[ToCanonical[Plus@@#]&,Partition[Quiet[AppendTo[List@@(equation//Expand),0]],Length[Kernels[]]]]]


(* ::Subsection:: *)
(*3.2 xPert*)


(* ::Input::Initialization:: *)
ParallelPerturbation[equation_] := Module[{},Return[Plus@@ParallelMap[Perturbation[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelExpandPerturbation[equation_] := Module[{},Return[Plus@@ParallelMap[ExpandPerturbation[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelVarD[T_,covd_][equation_] := Module[{},Return[Plus@@ParallelMap[VarD[T,covd][#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]


(* ::Subsection:: *)
(*3.3 xCoba*)


(* ::Input::Initialization:: *)
ParallelToBasis[ch_][equation_] := Module[{},Return[Plus@@ParallelMap[ToBasis[ch][#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelComponentArray[equation_] := Module[{},Return[Plus@@ParallelMap[ComponentArray[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelTraceBasisDummy[equation_] := Module[{},Return[Plus@@ParallelMap[TraceBasisDummy[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]

ParallelToValues[equation_] := Module[{},Return[Plus@@ParallelMap[ToValues[#]&,Flatten[{(equation//Expand)/.Plus->List}]]]]


(* ::Text:: *)
(*Custom function: //ToBasis[ch]//ToValues//TraceBasisDummy//ToValues in parallel*)


(* ::Input::Initialization:: *)
ParallelInCoordinates[ch_][equation_]:=Module[{},Return[Plus@@ParallelMap[ToValues[TraceBasisDummy[ToValues[ToBasis[ch][#]]]]&,Flatten[{(equation//Expand)/.Plus->List}]]]]


(* ::Chapter:: *)
(*5. End*)


(* ::Subsection:: *)
(*5.1 End Package*)


(* ::Input:: *)
(*$Context*)


(* ::Input::Initialization:: *)
End[]


(* ::Input:: *)
(*$Context*)


(* ::Input::Initialization:: *)
EndPackage[]


(* ::Input:: *)
(*$Context*)
