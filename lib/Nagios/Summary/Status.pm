package Nagios::Summary::Status;

use Mouse;
use Storable;
use Data::Dumper;
use CHI;
use POSIX qw(strftime);

has 'cache_path' => ( is => 'rw', isa => 'Any' );
has 'cache_expires' => ( is => 'rw', isa => 'Any' );
    
sub BUILD {
    my ( $self, $p ) = @_;

    $self->{'cache_path'} = 'chi_cache' unless $self->{'cache_path'};
    $self->{'cache_expires'} = $self->{'cache_expires'} unless $self->{'cache_expires'};
    $self->{'cache'} = CHI->new( 'driver' => 'File', 'root_dir' => $self->{'cache_path'} );
    $self->{'overall_status'} = 0;
    $self->{'overall_acknowleged_status'} = 0;
    $self->{'summary'}                = ();
    $self->{'server_service_status'}  = ();
}

sub get_cache_keys {
    my ( $self, $p ) = @_;
    return $self->{'cache'}->get_keys;
}

sub get_from_cache {
    my ( $self, $p ) = @_;
    return $self->{'cache'}->get( $p->{'name'} );
}

sub read_from_cache {
    my ( $self, $p ) = @_;
    my $name = $p->{'name'};
    my $ref  = $self->{'cache'}->get($name);
    die "no data in cache for [$name], run update\n" unless $ref;
    return $ref;
}

sub store_to_cache {
    my ( $self, $p ) = @_;
    my $ref  = $p->{'ref'};
    my $name = $p->{'name'};
    $self->{'cache'}->set( $name, $ref, $self->{'cache_expires'} );
    $self->{$name} = $ref;
}

sub hosts_dat {
    my ( $self, $p ) = @_;
    return $self->read_from_cache( { 'name' => 'hosts' } );
}

sub status_dat {
    my ( $self, $p ) = @_;
    return $self->read_from_cache( { 'name' => 'status' } );
}

sub nagios_log {
    my ( $self, $p ) = @_;
    return $self->read_from_cache( { 'name' => 'nagios_log' } );
}

sub hosts {
    my ( $self, $p ) = @_;
    my @hosts = ();
    my $ref   = $self->{'cache'}->get('server_service_status');
    foreach my $key ( keys( %{$ref} ) ) {
        push @hosts, $key;
    }
    return \@hosts;
}

sub groups {
    my ( $self, $p ) = @_;
    my @groups = ();
    my $ref    = $self->{'cache'}->get('summary');
    foreach my $key ( keys( %{ $ref->{'groups'} } ) ) {
        push @groups, $key;
    }
    return \@groups;
}

sub group {
    my ( $self, $p ) = @_;
    my $group = $p->{'name'};
    my $ref   = $self->{'cache'}->get('summary');
    return $ref->{'groups'}->{$group};
}

sub status {
    my ( $self, $p ) = @_;
    my $ref    = $self->{'cache'}->get('summary');
    my %status = ();
    foreach my $key ( keys(%$ref) ) {
        next if ( $key eq 'groups' );
        $status{$key} = $ref->{$key};
    }
    return \%status;
}

sub host {
    my ( $self, $p ) = @_;

    my $host = $p->{'host'};
    die "host method called but no host name specified" unless $host;

    my $get = $p->{'get'} || 'host_status';
    my $service = $p->{'service'};

    my $accepted_get_types =
      'host_values|service_values|service_value|service_list';
    die "host method called with parameter "
      . "get, not of type $accepted_get_types\n"
      unless ( $get =~ m{(?:$accepted_get_types)} );
  HOST:
    my $ref = $self->{'cache'}->get('server_service_status');
    foreach my $key ( keys( %{$ref} ) ) {
        if ( $key eq $host ) {
            if ( $get eq 'service_list' ) {
                my @services =
                  keys( %{ $ref->{$host}->{'service_status'} } );
                return \@services;
            }
            if ( $get eq 'service_value' ) {
                die "host method called with service_value but no service"
                  unless $service;
                return $ref->{$host}->{'service_status'}->{$service};
                return;
            }

            if ( $get eq 'service_values' ) {
                return $ref->{$host}->{'service_status'};
            }
            elsif ( $get eq 'host_values' ) {
                return $ref->{$host}->{'host_status'};
            }
            last HOST;
        }
    }
}

sub hosts_dat_parser {

    my ( $self, $p ) = @_;
    my $hosts_dat_file = $p->{'file'};
    open my $if, "<", $hosts_dat_file
      or die "can't open $hosts_dat_file for read : $!\n";
    my $hosts_data = '';
    while (<$if>) {
        $hosts_data .= $_;
    }

    close $if;

    my %hosts = ();

    while ( $hosts_data =~ m/ define \s+ (\S+) \s* { (.+?) }/msxg ) {

        my $type     = $1;
        my $info_dat = $2;

        my @lines = split( "\n", $info_dat );

        my %data = ();
        foreach my $l (@lines) {
            next if ( $l =~ m{^$} );

            if ( $l =~ m{ ^ \s+ (\S+?) \s+ ( .+ ) }msx ) {
                $data{$1} = $2;
            }
        }
        push @{ $hosts{$type} }, {%data};

    }

    $self->{'cache'}->set( 'hosts', \%hosts, $self->{'cache_expires'} );

}

sub status_dat_parser {

    my ( $self, $p ) = @_;
    my $status_dat_file = $p->{'file'};

    open my $if, "<", $status_dat_file
      or die "can't open $status_dat_file for read : $!\n";
    my $status_data = '';

    while (<$if>) {
        $status_data .= $_;
    }
    close $if;

    my %status = ();

    while (
        $status_data =~ m/ ( 
                            contactstatus | 
                            hostcomment | 
                            hoststatus | 
                            info | 
                            programstatus | 
                            servicecomment | 
                            servicestatus 
                            ) \s* { (.+?) }/msxg
         )
    {

        my $type     = $1;
        my $info_dat = $2;

        my @lines = split( "\n", $info_dat );

        my %data = ();
        foreach my $l (@lines) {
            next if ( $l =~ m{^$} );

            if ( $l =~ m{ ^ \s+ (.+?) = ( .* ) }msx ) {
                $data{$1} = $2;
            }
        }
        push @{ $status{$type} }, {%data};

    }

    $self->{'cache'}->set( 'status', \%status, $self->{'cache_expires'} );
}

sub generate_summary {
    my $self = shift;
    $self->{'config_hashref'} = $self->hosts_dat();
    $self->init_summary_hash();
    $self->get_combined_status();
    $self->save_summary_to_cache();
}

sub init_summary_hash {
    my $self = shift;
    $self->{'summary'}->{'status'}      = 0;
    $self->{'summary'}->{'acknowleged'} = 0;

    foreach my $ref ( @{ $self->{'config_hashref'}->{'host'} } ) {
        if ( $ref->{'hostgroups'} ) {
            my $hostgroups_string = $ref->{'hostgroups'};
            my @hostgroups = split( /,\s*/, $hostgroups_string );
            foreach my $grp (@hostgroups) {
                $self->{'summary'}->{'groups'}->{$grp}->{'status'} = 0;
            }
        }
    }
}

sub get_combined_status {
    my $self = shift;    

    
    foreach my $ref ( @{ $self->{'config_hashref'}->{'host'} } ) {

        next unless $ref->{'host_name'};
        my @hostgroups = split( ", *", $ref->{'hostgroups'} );

        my (
            $combined_status,   $combined_in_error_count,
            $acknowleged_count, $host_status_ref,
            $service_status_ref
        ) = $self->get_status( { 'host' => $ref->{'host_name'} } );

        $self->{'server_service_status'}->{ $ref->{'host_name'} }->{'host_status'} =
          $host_status_ref;
        $self->{'server_service_status'}->{ $ref->{'host_name'} }->{'service_status'} =
          $service_status_ref;

        $self->{'summary'}->{'acknowleged'} += $acknowleged_count if $acknowleged_count;
        $self->{'summary'}->{'in_error_count'} += $combined_in_error_count
          if $combined_in_error_count;

        # update for this group combined statuses
        foreach my $grp (@hostgroups) {
            if ( defined( $self->{'summary'}->{'groups'}->{$grp}->{'status'} ) ) {
                if ( $combined_status > $self->{'summary'}->{'groups'}->{$grp}->{'status'} )
                {
                    $self->{'summary'}->{'groups'}->{$grp}->{'status'} = $combined_status;
                }
                $self->{'summary'}->{'groups'}->{$grp}->{'combined_in_error_count'} +=
                  $combined_in_error_count
                  if $combined_in_error_count;
                $self->{'summary'}->{'groups'}->{$grp}->{'combined_acknowleged_count'} +=
                  $acknowleged_count;
            }
        }
    }
}

sub save_summary_to_cache {
    my $self = shift;
    $self->store_to_cache(
        {
            'ref'  => $self->{'summary'},
            'name' => 'summary'
        }
    );

    $self->store_to_cache(
        {
            'ref'  => $self->{'server_service_status'},
            'name' => 'server_service_status'
        }
    );
}

sub get_status {

    my ($self, $p) = @_;
    my $host_name = $p->{'host'};
    return unless $host_name;

    my %service_status          = ();
    my $current_state           = 0;
    my $combined_state          = 0;
    my $combined_in_error_count = 0;
    my $acknowleged_count       = 0;
    my $status_hashref          = $self->status_dat;

    # HOST STATUS
    my $host_status_summary = ();
    (
        $current_state,           $combined_state,
        $combined_in_error_count, $host_status_summary
      )
      = $self->get_host_status(
        {
            'status_ref'     => $status_hashref,
            'host'           => $host_name,
            'current_state'  => $current_state,
            'combined_state' => $combined_state
        }
      );

    # HOST SERVICE STATUS
    my ( $acknowledged_service_count, $combined_in_error_service_count,
        $service_status_summary )
      = $self->get_service_status(
        {
            'status_ref'     => $status_hashref,
            'host'           => $host_name,
            'current_state'  => $current_state,
            'combined_state' => $combined_state
        }
      );
    $combined_state = $combined_in_error_service_count
      if ( $combined_in_error_service_count > $combined_state );
    $acknowleged_count += $acknowledged_service_count
      if $acknowledged_service_count;
    $combined_in_error_count += $combined_in_error_service_count
      if $combined_in_error_service_count;

    return (
        $combined_state,    $combined_in_error_count,
        $acknowleged_count, $host_status_summary,
        $service_status_summary
    );

}

sub get_service_status {

    my ($self, $p) = @_;

    my $status_hashref          = $p->{'status_ref'};
    my $service_status_hashref  = $p->{'service_ref'};
    my $host_name               = $p->{'host'};
    my $current_state           = $p->{'current_state'};
    my $combined_state          = $p->{'combined_state'};
    my $acknowleged_count       = 0;
    my $combined_in_error_count = 0;
    my %srv_summ_hash           = ();
    foreach my $ref ( @{ $status_hashref->{'servicestatus'} } ) {

        if ( $ref->{'host_name'} eq $host_name ) {

            my $desc = $ref->{'service_description'};
            $srv_summ_hash{$desc}->{'plugin_output'} = $ref->{'plugin_output'};
            $srv_summ_hash{$desc}->{'performance_data'} =
              $ref->{'performance_data'};
            $current_state = $ref->{'current_state'};
            $srv_summ_hash{$desc}->{'current_state'} = $current_state;
            my $problem_has_been_acknowledged =
              $ref->{'problem_has_been_acknowledged'};
            if ($problem_has_been_acknowledged) {
                $acknowleged_count++;
            }
            $srv_summ_hash{$desc}->{'problem_has_been_acknowledged'} =
              $problem_has_been_acknowledged;
            $srv_summ_hash{$desc}->{'scheduled_downtime_depth'} =
              $ref->{'scheduled_downtime_depth'};
            $srv_summ_hash{$desc}->{'last_state_change'} =
              $ref->{'last_state_change'};
            if ( $current_state > 0 ) {
                $combined_in_error_count++;
            }
            $self->{'summary'}->{'status'} = $current_state
              if ( $current_state > $self->{'summary'}->{'status'} );

            if ( $current_state > $combined_state ) {
                $combined_state = $current_state;
            }

        }

    }
    return ( $current_state, $combined_state, \%srv_summ_hash );
}

sub get_host_status {

    my ($self, $p) = @_;

    my $status_hashref          = $p->{'status_ref'};
    my $host_name               = $p->{'host'};
    my $current_state           = $p->{'current_state'};
    my $combined_state          = $p->{'combined_state'};
    my %host_status             = ();
    my $acknowleged_count       = 0;
    my $combined_in_error_count = 0;
    my %srv_summ_hash           = ();

  HOSTSTATUS:
    foreach my $ref ( @{ $status_hashref->{'hoststatus'} } ) {
        if ( $ref->{'host_name'} eq $host_name ) {
            my $current_state = $ref->{'current_server_state'};
            $host_status{'current_server_state'} =
              $ref->{'current_server_state'};
            my $problem_has_been_acknowledged =
              $ref->{'problem_has_been_acknowledged'};
            if ($problem_has_been_acknowledged) {
                $acknowleged_count++;
            }
            $host_status{'problem_has_been_acknowledged'} =
              $problem_has_been_acknowledged;
            if ($problem_has_been_acknowledged) {
                $acknowleged_count++;
            }
            $host_status{'scheduled_downtime_depth'} =
              $ref->{'scheduled_downtime_depth'};
            $host_status{'performance_data'} = $ref->{'performance_data'};
            my $current_server_state = $ref->{'current_state'} . "\n";
            if ($current_state) {
                if ( $current_state > 0 ) {
                    $combined_in_error_count++;
                }
                $self->{'summary'}->{'status'} = $current_state
                  if ( $current_state > $self->{'summary'}->{'status'} );
                if ( $current_state > $combined_state ) {
                    $combined_state = $current_state;
                }
            }
            last HOSTSTATUS;
        }
    }
    return (
        $current_state,           $combined_state,
        $combined_in_error_count, \%host_status
    );
}

sub nagios_dat_parser {
    my ( $self, $p ) = @_;
    my $nagios_dat_file = $p->{'file'};
    open my $if, "<", $nagios_dat_file
      or die "can't open $nagios_dat_file for read : $!\n";
    $self->{'nagios_log'} = ();
    my @nagios_log = ();
  LINE:
    while (<$if>) {
        my $log_line = $_;
        if ( $log_line =~ m{ \[ (\d+) \] \s (.+?) : \s (.+?) \n }msx ) {
            my ( $epoch, $label, $entry ) = ( $1, $2, $3 );
            if (   ( $label =~ m{CURRENT \s (?:SERVICE|HOST) \s STATE }msx )
                || ( $label =~ m{LOG \s (?:ROTATION|VERSION)}msx ) )
            {
                next LINE;
            }
            my $now_string = strftime "%Y%m%d-%H%M%S", localtime($epoch);
            if ( $now_string && $label && $entry ) {
                push @nagios_log,
                    {
                        'date_time' => $now_string,
                        'label'     => $label,
                        'entry'     => $entry
                    };
               
            }
        }
    }
    close $if;
    $self->{'cache'}->set( 'nagios', \@nagios_log, $self->{'cache_expires'} );
}


#################### main pod documentation begins ###################

=head1 NAME

Nagios::Summary::Status - Creates summaries from a nagios status, host config and nagios log file storing output to CHI ( formerly Cache::Cache ) file store. 

=head1 SYNOPSIS

  use Nagios::Summary::Status;
  my $ns = Nagios::Summary::Status->new({'cache_path' => 'my_cache_files', 'cache_expires' => '2 minutes'});
  $ns->hosts_dat_parser({'file' => 'hosts.cfg'});
  $ns->status_dat_parser({'file' => 'status.dat'});
  $ns->generate_summary();
  $ns->nagios_dat_parser({'file' => 'nagios.log'});

=head1 DESCRIPTION

Given 3 input files, hosts.cfg, status.dat and nagios.log, summary of status and log data is produced
and stored to a 'CHI' cache.

A cache is used so that repeated calls to dump data need not require the config files to be re-parsed for a default of '2 minutes'

A dump script - see USAGE can be used to extract the time limited cache data. 

=head1 USAGE

The following is a dump script. In this initial release, Data::Dumper is the most usefull way to extract data from the CHI cache

It is intended that new methods enabling the distribution of data via JSONP used as part of a web service will added in time. In particular,
time based re-parse and store to cache to be triggered on expiry of the cache.

the following 'update.pl' script :

  #!/usr/bin/env perl

  use strict;
  use warnings;
  use Nagios::Summary::Status;

  my $ns = Nagios::Summary::Status->new({'cache_path' => 'my_cache_files2', 'cache_expires' => '2 minutes'});
  $ns->hosts_dat_parser({'file' => 'hosts.cfg'});
  $ns->status_dat_parser({'file' => 'status.dat'});
  $ns->generate_summary();
  $ns->nagios_dat_parser({'file' => 'nagios.log'});

the following 'dump_cache.pl' script :

  #!/usr/bin/env perl

  use strict;
  use warnings;
  use Nagios::Summary::Status;
  use Data::Dumper;

  my $ns = Nagios::Summary::Status->new({'cache_path' => 'my_cache_files', 'cache_expires' => '2 minutes'});
  my @keys = $ns->get_cache_keys();
  my $names = join('|', @keys);
  my $name = shift;
  die "no cache name specified : [$names]\n" unless $name;
  die "no cache name specified : [$names]\n" unless ($name =~ m{$names});
  my $ref = $ns->get_from_cache({ 'name' => $name });

  print Dumper($ref);

output from the above will look something like :

    ./dump_cache.pl 
    no cache name specified : [nagios|hosts|status|server_service_status|summary]

the five cache names are available and the following will dump 'summary' data:

  ./update.pl ; ./dump_cache.pl summary

=head1 BUGS

=head1 SUPPORT

=head1 AUTHOR

    Jon Brookes
    CPAN ID: JNBROOKES
    AJBC Ltd
    marshyon (at) gmail.com
    http://ajbcontracts.co.uk

=head1 COPYRIGHT

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.


=head1 SEE ALSO

perl(1).

=cut

#################### main pod documentation ends ###################


1;
# The preceding line will help the module return a true value

