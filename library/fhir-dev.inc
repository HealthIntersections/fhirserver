// there's a bug in the lazarus/FPC compiler where global settings 
// such as assertions are not consistently the same across all units. 
// this resets the global compile settings for development MODE

// these settings are intended to be consistent between code and compile options in the project settings

// do not make changes in the file fhir-status.inc - it is overwritten by either fhir-dev.inc or fhir-prod.inc by the build scripts 

// THIS IS DEVELOPMENT MODE

{$C+} // assertions on - this is the important one - turns object tracking on and off consistently
{$I+} // IO checking on - though this is probably useless?
{$Q-} // overflow checking off - these are always off; doesn't run with them on
{$R-} // range checking off - these are always off; doesn't run with them on
{$OPTIMIZATION OFF} // all optimizations off for development
{$D+} // debugging info on for development

{
The base class TFslObject can track all instantiated objects.
Doing so is useful for leak hunting in production, but is also a little costly.
Enable it with this define
}
{$DEFINE OBJECT_TRACKING}



