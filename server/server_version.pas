unit server_version;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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


interface

const
  ServerDBVersionEarliestSupported = 12;

//  ServerDBVersion = 3;
//  ServerDBVersion = 4; // added secure flag in versions table
//  ServerDBVersion = 5; // added scores to search entries table
//  ServerDBVersion = 6; // added reverse to search entries table
//  ServerDBVersion = 7; // changed compartment table. breaking change
//  ServerDBVersion = 8; // added ImplementationGuide column to Types table
//  ServerDBVersion = 9; // added Observations Table
//  ServerDBVersion = 10; // added ForTesting flag
//  ServerDBVersion = 11; // added ResourcePreviousVersion field to SubscriptionQueue
//  ServerDBVersion = 12; // rework Observations Table (can't do this as an upgrade)
//  ServerDBVersion = 13; // add Observations.ConceptList
//  ServerDBVersion = 14; // add Authorizations
//   ServerDBVersion = 15; // add Uuid to Authorizations
//  ServerDBVersion = 16; // add PatientId to Authorizations
//  ServerDBVersion = 17; // add AuthorizationSessions and Connections
//  ServerDBVersion = 18; // add AsyncTasks
//  ServerDBVersion = 19; // add RegisteredClients
//  ServerDBVersion = 20; // add PseudoData
//  ServerDBVersion = 21; // add ClientRegistrations.PatientContext
//  ServerDBVersion = 22; // add AsyncTasks.Request and AsyncTasks.TransactionTime
//  ServerDBVersion = 24; // add AsyncTasks.Secure

//  ServerDBVersion = 26; // add Twilio Proxy

//  ServerDBVersion = 27; // add Package download count

//  ServerDBVersion = 28; // add Package token tracking

//  ServerDBVersion = 29; // add Loaded Package tracking

//  ServerDBVersion = 30; // add OAuthLogin Launch field

  ServerDBVersion = 31; // make OAuthLogin.Scope longer (1024)


implementation
end.
