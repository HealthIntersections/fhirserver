
{$I DICompilers.inc}

{ ---------------------------------------------------------------------------- }
{ General                                                                      }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_Use_Wide_Char_Set_Consts}// Default: Off
{ If defined, code will use constants of WideChar sets. WideChar set constants
  will, however, cause a compiler error when compiled with TD32 debug info.
  This error is present in D4, D5, D6 and D7. }

{.$DEFINE DI_No_Range_Checking}// Default: Off
{ Defining DI_No_Range_Checking will suppress generation of code which checks
  for ranges errors. No error messages will be thrown in case of violations.
  Enabeling DI_No_Range_Checking generates faster and smaller code. }

{.$DEFINE DI_No_Classes}// Default: Off
{ If defined, The Delphi Inspiration libraries try to avoid using and linking
  Delphi's Classes.pas. Not using Classes.pas usually results in a 15-17 KB
  smaller exe file size for console applications. For Delphi GUI applications,
  turning on DI_No_Classes may actually create larger file sizes. }

{$DEFINE FLOAT_EXTENDED_PRECISION} // Default
{.$DEFINE FLOAT_DOUBLE_PRECISION}
{.$DEFINE FLOAT_SINGLE_PRECISION}
{ Math precision selection, mutually exclusive. }

{.$DEFINE DI_No_Win_9x_Support}// Default: Off
{ Define DI_No_Win_9x_Support if you want to compile your application without
  support for the Win9x platforms Windows 95 and Windows 98. They will then
  run only on Windows NT / Windows 2000 / Windows XP. }

{.$DEFINE DI_Show_Hints}// Default: Off
{ Generation of hints has been disabled for code which is known to be safe.
  To enable these hints, turn on DI_Show_Hints. }

{.$DEFINE DI_Show_Warnings}// Default: Off
{ Generation of warnings has been disabled for code which is known to be safe.
  To enable these warnings, turn on DI_Show_Warnings. }

{ ---------------------------------------------------------------------------- }
{ DIContainers                                                                 }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_No_Containers_Assign}// Default: Off
{ If defined, the general assignment capabilities will not be compiled into
  DIContainers. Enabeling DI_No_Containers_Assign will generate smaller exe
  files.

  This option is only available for the DIContainers source code. }

{.$DEFINE DI_No_Containers_Persistent}// Default: Off
{ If DI_No_Containers_Persistent is defined, all DIContainers will inherit
  directly from TObject instead of TPersistent and they will not have
  full streaming support in the IDE. Enabeling DI_No_Containers_Persistent will
  generate smaller exe files.

  This option is only available for the DIContainers source code. }

{.$DEFINE DIC_Design}// Default: Off
{ This option is only relevant for modifications to the the DIContainers source code. }

//------------------------------------------------------------------------------
// DIConverters
//------------------------------------------------------------------------------

{$IFDEF CLR}

{$UNDEF DIICONV}
{$UNDEF DICONVERTERS}

{$ENDIF CLR}

{$IFDEF USE_DIICONV}
{$DEFINE USE_DIIC}
{$ENDIF}

{$IFDEF USE_DICONVERTERS}
{$DEFINE USE_DIIC}
{$ENDIF}

{ ---------------------------------------------------------------------------- }
{ DIFileFinder                                                                 }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_No_FileFinder_Component}// Default: Off
{ If DI_No_FileFinder_Component is defined, TDIFileFinder inherites from TObject
  instead of TComponent. Enabling DI_No_FileFinder_Component creates smaller
  executable files, even more so if Classes.pas is not used. This can be
  useful for console or CGI applications.

  This option is only available for the DIFileFinder source code. }

{ ---------------------------------------------------------------------------- }
{ DIRegEx                                                                      }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_No_RegEx_Component}// Default: Off
{ If DI_No_Pcre_Component is defined, TDIPcre inherites from TObject
  instead of TComponent. Enabling DI_No_Pcre_Component creates smaller
  executable files, even more so if Classes.pas is not used. This can be
  useful for console applications or CGI / ISAPI DLLs.

  This option is only available for the DIPcre source code. }

{$IFDEF DI_No_Pcre_Component}
{$DEFINE DI_No_RegEx_Component}
{$ENDIF DI_No_Pcre_Component}
{ Legacy / Backwards compatibility. }

{.$DEFINE DI_No_Pcre_Range_Checking}// Default: Off
{ Defining DI_No_Pcre_Range_Checking will suppress generation of code which
  checks for ranges errors. No error messages will be thrown in case of
  violations. Enabeling DI_No_Pcre_Range_Checking generates faster and smaller
  code.

  This option is only available for the DIPcre source code. }

{ ---------------------------------------------------------------------------- }
{ DIUnicode                                                                    }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_No_Unicode_Component}// Default: Off
{ If DI_No_Unicode_Component is defined, the DIUnicode classes inherit from
  TObject instead of TComponent. Enabling DI_No_Unicode_Component creates
  smaller executable files, even more so if Classes.pas is not used. This can be
  useful for console applications or CGI / ISAPI DLLs.

  This option is only available for the DIUnicode source code. }

{ ---------------------------------------------------------------------------- }
{ DIZipWriter                                                                  }
{ ---------------------------------------------------------------------------- }

{.$DEFINE DI_No_ZipWriter_Component}// Default: Off
{ If DI_No_ZipWriter_Component is defined, TDIZipWriter inherites from TObject
  instead of TComponent. Enabling DI_No_ZipWriter_Component creates smaller
  executable files, even more so if Classes.pas is not used. This can be
  useful for console applications or CGI / ISAPI DLLs.

  This option is only available for the DIZipWriter source code. }

{ ---------------------------------------------------------------------------- }
{ Do not modify below here.                                                    }
{ ---------------------------------------------------------------------------- }

{ Check if one of the components requires Classes.pas
  and undefine DI_No_Classes accordingly. }

{$IFNDEF DI_No_Containers_Persistent}
{$UNDEF DI_No_Classes} // TPersistent is defined in Classes.pas.
{$ENDIF}

{$IFNDEF DI_No_FileFinder_component}
{$UNDEF DI_No_Classes} // TComponent is defined in Classes.pas.
{$ENDIF}

{$IFDEF DI_No_PCRE_component} // Backwards compatibility.
{$DEFINE DI_No_RegEx_component}
{$ENDIF DI_No_PCRE_component}

{$IFNDEF DI_No_RegEx_component}
{$UNDEF DI_No_Classes} // TComponent is defined in Classes.pas.
{$ENDIF}

{$IFNDEF DI_No_Unicode_component}
{$UNDEF DI_No_Classes} // TComponent is defined in Classes.pas.
{$ENDIF}

{$IFNDEF DI_No_ZipWriter_component}
{$UNDEF DI_No_Classes} // TComponent is defined in Classes.pas.
{$ENDIF}

