unit FHIR.Server.Version;

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
  ServerDBVersion = 23; // add AsyncTasks.Secure

implementation
end.
