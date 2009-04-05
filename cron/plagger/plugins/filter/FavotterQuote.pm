package Plagger::Plugin::Filter::FavotterQuote;
use strict;
use base qw( Plagger::Plugin );

use Encode;

sub register {
    my ($self, $context) = @_;
    $context->register_hook(
        $self,
        'update.entry.fixup' => \&filter,
    );
}

sub filter {
    my ($self, $context, $args) = @_;
    
    my $body = $args->{entry}->{body};
    $body =~ /^.+?<\/img>(.+?)<\/a> (.+?)<br \/>/o;
    my $id = $1;
    my $quote = $2;
    my $title = "Twitter / " . $id;
    
    $args->{entry}->title($title);
    $args->{entry}->body($quote);
}

1;

__END__

=head1 NAME

Plagger::Plugin::Filter::FavotterQuote - Making Favotter feed to Tumblr Quote.

=head1 SYNOPSIS

  - module: Filter::FavotterQuote

=head1 DESCRIPTION

This plugin makes favotter feed to tumblr quote.
L<http://favotter.matope.com/>.

=head1 AUTHOR

riywo

=head1 SEE ALSO

L<Plagger>

=cut
