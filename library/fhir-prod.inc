// there's a bug in the lazarus/FPC compiler where global settings 
// such as assertions are not consistently the same across all units. 
// this resets the global compile settings for development MODE

// these settings are intended to be consistent between code and compile options in the project settings

// do not make changes in the file fhir-status.inc - it is overwritten by either fhir-dev.inc or fhir-prod.inc by the build scripts 

{$C-} // assertions off - this is the important one - turns object tracking off consistently
{$I-} // IO checking off 
{$Q-} // overflow checking off - these are always off; doesn't run with them on
{$R-} // range checking off - these are always off; doesn't run with them on
{$OPTIMIZATION LEVEL3} // level 3 optimizations for production
{$D-} // debugging info off for production
