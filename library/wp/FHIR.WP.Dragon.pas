Unit FHIR.WP.Dragon;
{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

Interface


Uses
  fsl_base;


Type
  TDragonPlaybackManager = Class(TFslObject)
    Public
      Procedure Open; Overload; Virtual;
      Procedure Close; Overload; Virtual;

      Procedure CloseCorrectionDialog; Virtual;

      Procedure Playback; Overload; Virtual;
      Procedure Stop; Overload; Virtual;
      Procedure Accelerate; Overload; Virtual;
      Procedure Deccelerate; Overload; Virtual;

      Procedure SaveSession(Const sFilename : String); Overload; Virtual;
      Procedure LoadSession(Const sFilename : String); Overload; Virtual;
  End;

  TDragonPlaybackManagerClass = Class Of TDragonPlaybackManager;


Implementation


Procedure TDragonPlaybackManager.Open;
Begin
End;


Procedure TDragonPlaybackManager.Close;
Begin
End;


Procedure TDragonPlaybackManager.Playback;
Begin
End;


Procedure TDragonPlaybackManager.Stop;
Begin
End;


Procedure TDragonPlaybackManager.Accelerate;
Begin
End;


Procedure TDragonPlaybackManager.Deccelerate;
Begin
End;


Procedure TDragonPlaybackManager.SaveSession(Const sFilename : String);
Begin
End;


Procedure TDragonPlaybackManager.LoadSession(Const sFilename : String);
Begin
End;


Procedure TDragonPlaybackManager.CloseCorrectionDialog;
Begin
End;


End.

