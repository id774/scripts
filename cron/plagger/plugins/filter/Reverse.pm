package Plagger::Plugin::Filter::Reverse;
use strict;
use base qw( Plagger::Plugin );

sub register {
    my($self, $context) = @_;
    $context->register_hook(
        $self,
        'update.feed.fixup' => \&feed
	);
}

sub feed {
    my($self, $context, $args) = @_;

    $context->log(debug => "reverse");
    my @entries = $args->{feed}->entries;
    @entries = reverse(@entries);
    $args->{feed}->{entries} = \@entries;
}

1;

__END__

=head1 NAME

Plagger::Plugin::Filter::Reverse - Reverse Sorting

=head1 SYNOPSIS

  - module: Filter::Reverse

=head1 DESCRIPTION

This Filter makes the feed reverse.
This is originary published here L<http://d.hatena.ne.jp/nirvash/20060511/1147298244>,
and copied by riywo.

=head1 AUTHOR

nirvash

=head1 SEE ALSO

L<Plagger>

=cut
