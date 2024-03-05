// there's a bug in the lazarus/FPC compiler where global settings 
// such as assertions are not consistently the same across all units. 
// this resets the global compile settings for development MODE

// do not make changes in the file fhir-status.inc - it is overwritten by either fhir-dev.inc or fhir-prod.inc by the build scripts 

{$C+} // assertions on
{$I+} // IO checking on
{$Q+} // overflow checking on
{$R+} // range checking on 
