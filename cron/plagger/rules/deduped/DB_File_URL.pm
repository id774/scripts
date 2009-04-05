package Plagger::Rule::Deduped::DB_File_URL;
use strict;
use base qw( Plagger::Rule::Deduped::Base );

use DB_File;

sub id_for {
    my($self, $entry) = @_;
    
    return $entry->permalink;
}

sub init {
    my($self, $rule) = @_;
    $self->{path} = $rule->{path} || Plagger->context->cache->path_to('Deduped.db');
    $self->{db} = tie my %cache, 'DB_File', $self->{path}, O_RDWR|O_CREAT, 0666, $DB_HASH
        or Plagger->context->error("Can't open DB_File $self->{path}: $!");
}

sub find_entry {
    my($self, $url) = @_;
    
    my $status = $self->{db}->get($url, my $value);
    return if $status == 1; # not found
    
    return $value;
}

sub create_entry {
    my($self, $url, $digest) = @_;
    $self->{db}->put($url, $digest);
}

1;
__END__

=head1 NAME

Plagger::Rule::Deduped::DB_File_URL -

=head1 SYNOPSIS

  - module: Filter::Rule
    rule:
      module: Deduped
      engine: DB_File_URL
      path: cache.db

=head1 DESCRIPTION

Plagger::Rule::Module::DB_File_URL

=head1 AUTHOR

manabou

=head1 SEE ALSO

L<Plagger>L<http://d.hatena.ne.jp/manabou/20070208/1170943271>

=cut
