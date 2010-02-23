package Plagger::Plugin::Filter::TwitterOnly;
use strict;
use warnings;
use base qw (Plagger::Plugin);

our $VERSION = '0.01';

sub register {
    my ($self, $context) = @_;
    $context->register_hook(
        $self,
        'update.feed.fixup' => \&filter,
    );
}

sub filter {
    my ($self, $context, $args) = @_;
    for my $entry ($args->{feed}->entries) {
        if ($entry->link !~ /^http:\/\/twitter\.com\//) {
            $context->log(info => "Delete entry " . $entry->link);
            $args->{feed}->delete_entry($entry);
        }
    }
}

1;

__END__

=head1 NAME

Plagger::Plugin::Filter::TwitterOnly - Filtering Twitter Only

=head1 SYNOPSIS

    - module: Filter::TwitterOnly

=head1 DESCRIPTION

Update your feed to twitter only.

=head1 AUTHOR

774

=head1 SEE ALSO

L<Plagger>

=cut
