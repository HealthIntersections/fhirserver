delete from AsyncTasks
delete from AuthorizationSessions
delete from Authorizations
delete from ClientRegistrations
delete from IndexEntries
delete from indexes
delete from ObservationQueue
delete from ObservationCodes
delete from Observations
delete from LoadedPackages
delete from NotificationQueue
delete from SubscriptionQueue
delete from PackageFHIRVersions
delete from PackageDependencies
delete from PackagePermissions
delete from PackageVersions
delete from Packages
delete from Twilio
delete from SearchEntries
delete from Searches
delete from PseudoData
delete from Spaces
delete from OAuthLogins

update ids set MasterResourceKey = null
ALTER TABLE Ids DROP CONSTRAINT FK_ResCurrent_VersionKey

delete from VersionTags

delete from Tags
delete from Versions
delete from Ids
delete from Compartments
delete from Types
delete from UserIndexes
delete from Users


