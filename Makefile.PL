

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME         => 'Nagios::Summary::Status',
    VERSION_FROM => 'lib/Nagios/Summary/Status.pm', # finds \$VERSION
    AUTHOR       => 'Jon Brookes (marshyon (at) gmail.com)',
    ABSTRACT     => 'Creates summares from nagios a nagios status, host config and nagios log file storing output to CHI ( formerly Cache::Cache ) file store. ',
    PREREQ_PM    => {
                     'Test::Simple' => 0.44,
                    },
);
