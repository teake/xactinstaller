(* Mathematica Package *)

(*

:Title: 	

	xActInstaller

:Context: 	

	xActInstaller` 

:Author: 

	Teake Nutma

:Mathematica Versions:

	8 - 9

:License: 

	GPL

:Installation:

	This package can be invoked without local installation by 
	Import["https://raw.github.com/teake/xactinstaller/master/xActInstaller/xActInstaller.m"];

:Discussion:

	Downloads and installs xAct and sub-packages automatically.

:Example usage:

		InstallxAct["1.0.5"]

	installs version 1.0.5 of xAct locally.

		InstallPackage["packageName", "packageVersion", "zipURL"]

	installs the latest version of xAct and also the given package from the zipURL.

:TODO:

	* Keep the Invar database when installing a new xAct version.
	
*)


(* First, get Rolf Mertig's CopyRemote package. *)
Import["https://copyremote.googlecode.com/hg/CopyRemote/CopyRemote.m"]


(***********************************
 *                                 *
 *         Begin package           *
 *                                 *
 ***********************************)
 
 
BeginPackage["xActInstaller`"]

InstallxAct::usage 	= 
	"InstallxAct[] installs the latest version of xAct.\n" <>
	"InstallxAct[version] installs the given version number of xAct.\n" <>
	"InstallxAct[version, dir] installs xAct into the given directory\n\n" <>
	"If a newer version is found locally, nothing is installed. " <>
	"Older versions are not overwritten but renamed.";

InstallPackage::usage = 
	"InstallPackage[name, version, url] installs the latest xAct version and the given package from the URL.";

Begin["`Private`"]


(***********************************
 *                                 *
 *     Cofiguration variables      *
 *                                 *
 ***********************************)


(* The default install directory. *)
defaultInstallDir = FileNameJoin @ { $UserBaseDirectory, "Applications" };


(* The URL of the main xAct bundle. *)
(* TODO: update this when needed. *)
xActZipURL[version_String] /; VersionGreaterEqual[version, "1.1.0"] :=
	StringJoin[ "http://xact.es/download/xAct_", version, ".tgz" ];
xActZipURL[version_String] :=
	StringJoin[ "http://xact.es/download/xAct_", version, If[$OperatingSystem === "Windows", ".zip", ".tgz"] ];


(* Lookup table for xAct version numbers. *) 
(* TODO: are these dates correct? *)
xCoreDateTable = {
	{ {2013,  1, 27}, "1.0.5"},
	{ {2012,  5,  5}, "1.0.4"},
	{ {0,     0,  0}, Null}
};

(* Make a function out of the xCoreDateTable that works for any date in the intervals. *)
xCoreDateToVersion[_] := Null;
Map[
	With[{tabledate = First @ #},
		xCoreDateToVersion[ date : {_Integer, _Integer, _Integer} ] /; OrderedQ @ {tabledate, date} = Last @ #
	]&
	,
	xCoreDateTable
];

(* The latest xAct version number. *)
xActLatest = Last @ First @ xCoreDateTable



(***********************************
 *                                 *
 *        Public functions         *
 *                                 *
 ***********************************)


Options[InstallPackage] ^= 
	{
		"xActVersion" 		-> "Latest",
		"ExtractPattern" 	-> Automatic,
		"RemoveItems" 		-> Automatic
	}

InstallPackage[ name_String, version_String, zipurl_String, options___Rule ] :=
	InstallPackage[name, version, zipurl, Default, options];

InstallPackage[ name_String, version_String, zipurl_String, installdir_, OptionsPattern[] ] :=
	Module[
		{
			xactversion 	= OptionValue["xActVersion"],
			extractpattern	= OptionValue["ExtractPattern"],
			removeitems 	= OptionValue["RemoveItems"],
			xactinfo		= GetxActInfo[],
			xactdir
		},
		
		If[	xactversion === "Latest",
			xactversion = xActLatest
		];
		
		If[	removeitems === Automatic,
			removeitems = { { "xAct", name } }
		];
		
		If[	extractpattern === Automatic,
			extractpattern = { "xAct", name, "*" }
		];
		
		xactdir = Last @ xactinfo;
		
		InstallPackages[
			{
				InstallxActArguments[ xactinfo, xactversion, installdir ],
				{
					name, 
					version, 
					zipurl, 
					If[ installdir === Default,
						If[ TrueQ @ Quiet @ DirectoryQ @ xactdir,
							ParentDirectory @ xactdir,
							defaultInstallDir
						],
						installdir
					], 
					"ExtractPattern" -> extractpattern, "RemoveItems" -> removeitems
				}
			}
		];
	];


InstallxAct[args___] := InstallPackages @ List @ InstallxActArguments[ GetxActInfo[], args ];



(***********************************
 *                                 *
 *     Main private functions      *
 *                                 *
 ***********************************)


(* Returns {"version", "installdir"} of the installed xAct version, 
   or {Null, Null} if not installed. *)
GetxActInfo[] := 
	Quiet @ Check[
		(* Try to load xCore and determine its date. *)
		Block[{Print}, 
			Get["xAct`xCore`"]
		];
		{
			xCoreDateToVersion @ Last @ xAct`xCore`$Version, 
			If[ TrueQ @ Quiet @ DirectoryQ @ xAct`xCore`$xActDirectory,
				xAct`xCore`$xActDirectory,
				Null
			]
		}
		,
		{Null, Null}
		,
		{Get::noopen}
	];


(* Returns Sequence[{"xAct", version, zipurl, installdir, "RemoveItems" -> "xAct" }], 
   or Sequence[] when there is an existing xAct install. *)

InstallxActArguments[ { oldversion_, olddir_} ] :=
	InstallxActArguments[ {oldversion, olddir}, xActLatest ];

InstallxActArguments[ { oldversion_, olddir_}, version_String ] :=
	InstallxActArguments[ {oldversion, olddir}, version, Default ];

InstallxActArguments[ { oldversion_, olddir_}, version_String, installdir_] :=
	Sequence @@ If[ VersionGreaterEqual[ oldversion, version ]
	,
		{}
	,
		{{
			"xAct",
			version,
			xActZipURL @ version,
			If[ installdir === Default,
				If[ TrueQ @ Quiet @ DirectoryQ @ olddir,
					ParentDirectory @ olddir,
					defaultInstallDir
				],
				installdir
			],
			"RemoveItems" -> {"xAct"}
		}}
	];


(* Installs a list of packages after giving the user a popup-dialog. *)
InstallPackages[packagelist : { {__}... }] := 
	Switch[
		InstallDialog @ packagelist
		,
		"OK",
		InstallPackageList @ packagelist
		,
		"ChooseDir",
		InstallPackages @ PackageListSetDir[
			packagelist,
			SystemDialogInput[
				"Directory", 
				PackageListGetDir @ packagelist, 
				WindowTitle -> "Choose base directory to install xAct into"
			]
		]
		,
		"Nothing",
		Null
		,
		_,
		$Canceled
	];


(*
 * Custom choice dialogs for the installation. 
 *)

(* "Nothing to install" dialog. *)
InstallDialog[{}] :=
	ChoiceDialog[
		"Nothing to install.",
		{"OK" -> "Nothing"},
		WindowTitle -> "xAct installer"
	];

(* Normal install dialog with directory change button. *)
InstallDialog[packagelist : { {__}... }] := 
	ChoiceDialog[
		StringJoin @ Riffle[
			{
				"The package(s)",
				Sequence @@ ( ( "  " <> #[[1]] <> " " <>  #[[2]] )& /@ packagelist ),
				"will be installed into the directory",
				"  " <> PackageListGetDir @ packagelist,
				"\nDo you want to continue?"
			}, 
			"\n"
		],
		{ "OK" -> "OK", "Choose directory" -> "ChooseDir", "Cancel" -> "Cancel" },
		WindowTitle -> "xAct installer"
	];  


(* Installs a list of packages *without* a popup dialog. *)
InstallPackageList[ packagelist : { {__}... } ] := 
	Module[
		{
			installed = TakeWhile[packagelist, ( InstallPackageList[ # ] =!= $Failed )&]
		},
		If[ Length @ installed === Length @ packagelist,
			Print["Installation finished."],
			$Failed
		];
	];

(* Installs a single package. *)
InstallPackageList[ {packagename_String, packageversion_String, installzipargs__} ] := 
	Module[
		{
			message = "Installing " <> packagename <> " " <> packageversion <> " ... ",
			result
		},
		Monitor[
			result = InstallZip @ installzipargs,
			message
		];
		If[ result === $Failed
			,
			Print[ message <> "failed."];
			Return @ $Failed
			,
			Print[ message <> "done."]
		];		
	];		
	


(*************************************
 *                                   *
 * General-purpose install functions *
 *                                   *
 *************************************)


(* Downloads a zip file from the internet, and upacks it to given directory. *)
Options[InstallZip] ^= 
	{
		"ExtractPattern" -> "*",
		"RemoveItems" -> {},
		"RemoveAction" -> "Rename"
	};
	
InstallZip[ zipurl_String, extractdir_?DirectoryQ, OptionsPattern[] ] := 
	Module[
		{
			localfile 		= CopyRemote`CopyRemote @ zipurl,
			extractpattern 	= FileNameJoin @ { Sequence @@ OptionValue @ "ExtractPattern" },
			removeitems		= OptionValue @ "RemoveItems",
			removeaction	= OptionValue @ "RemoveAction"
 		},
 		
 		If[ Not @ TrueQ @ Quiet @ FileExistsQ @ localfile,
 			Return @ $Failed
 		];
 		
 		If[ removeaction === "Rename",
 			removeaction = RenameItem,
 			removeaction = DeleteItem
 		];
 		
 		removeaction[ FileNameJoin @ {extractdir, Sequence@@#} ]& /@ removeitems; 
 		
		(* Download and install the remote zip file. *)
		Quiet @ Check[
			Monitor[
				ExtractArchive[localfile, extractdir, extractpattern, CreateIntermediateDirectories->True],
				"Unpacking " <> FileNameTake @ localfile <> " to " <> extractdir <> "."
			];
			DeleteFile @ localfile;
			,
			Return @ $Failed
		];
 	];

(* Deletes a file or directory. *)
DeleteItem[dir_?DirectoryQ] :=  
	Quiet @ DeleteDirectory[dir, DeleteContents -> True];
DeleteItem[file_] :=
	Quiet @ DeleteFile @ file;

(* Renames a file or folder "file.ext" to "file.ext_old", 
   or if that's taken, "file.ext_old1" etc. *)
RenameItem[file_?FileExistsQ, renameextension_String:"_old"] := 
	Module[
		{
			newfile, existing, basenew
		},
		basenew		= FileNameTake[file] <> renameextension;
		existing 	= FileNameTake /@ FileNames[ basenew ~~ NumberString..., {ParentDirectory @ file} ];
		newfile		= basenew <> If[
			Length @ existing == 0,
			"", 
			ToString[ 1 + ( ToExpression[ StringDrop[ Last @ existing, StringLength @ basenew ] ] /. Null -> 0 ) ]
		];
		newfile		= FileNameJoin @ { ParentDirectory @ file, newfile };
		RenameFile[ file, newfile ];
	];



(***********************************
 *                                 *
 *       Auxiliary functions       *
 *                                 *
 ***********************************)


(* Function that compares two version strings. *)
VersionGreaterEqual[v1_String, v2_String] :=
	OrderedQ @ PadRight @ Map[ToExpression, StringSplit[#, "."] & /@ {v2, v1}, {2}];
VersionGreaterEqual[Null, v2_String] :=
	VersionGreaterEqual["0", v2];
VersionGreaterEqual[v1_String, Null] :=
	VersionGreaterEqual[v1, "0"];


(* Get the installation directory from a list of packages. *)
PackageListGetDir[{ {_,_,_,dir_,___}, ___ }] := dir;
PackageListGetDir[___] := defaultInstallDir;

(* Set the installation directory on a list of packages. *)
PackageListSetDir[ packagelist : { {__}... }, dir_String ] := Module[{package = #}, package[[4]] = dir; package]& /@ packagelist;
PackageListSetDir[ packagelist_ , _ ] := packagelist;



(***********************************
 *                                 *
 *          End package            *
 *                                 *
 ***********************************)
 

End[]

EndPackage[]

