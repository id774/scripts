package Plagger::Plugin::Publish::Tweet;
use strict;
use base qw( Plagger::Plugin );

use Encode;
use Net::Twitter;
use Time::HiRes qw(sleep);

sub register {
    my($self, $context) = @_;
    $context->register_hook(
        $self,
        'publish.entry' => \&publish_entry,
        'plugin.init'   => \&initialize,
    );
}

sub initialize {
    my($self, $context) = @_;
    my %opt = (
        username => $self->conf->{username},
        password => $self->conf->{password},
    );
    for my $key (qw/ apihost apiurl apirealm/) {
        $opt{$key} = $self->conf->{$key} if $self->conf->{$key};
    }
    $self->{twitter} = Net::Twitter->new(%opt);
}

sub publish_entry {
    my($self, $context, $args) = @_;
    my $body = $args->{entry}->body_text;

    if ($self->conf->{templatize}) {
	    $body = $self->templatize($self->conf->{templatize}, $args);
    }

    $context->log(info => "Updating Twitter status to '$body'");
    $self->{twitter}->update( encode_utf8($body) ) or $context->error("Can't update twitter status");

    my $sleeping_time = $self->conf->{interval} || 15;
    $context->log(info => "sleep $sleeping_time.");
    sleep( $sleeping_time );
}

1;
__END__

=head1 NAME

Plagger::Plugin::Publish::Tweet - Update your tweet

=head1 SYNOPSIS

  - module: Publish::Tweet
    config:
      username: twitter-id
      password: twitter-password
      templatize: tweet.tt # Template-Toolkit filename

=head1 DESCRIPTION

This plugin sends feed entries summary to your Twitter account status.

=head1 CONFIG

=over 4

=item username

Twitter username. Required.

=item password

Twitter password. Required.

=item interval

Optional.

=item apiurl

OPTIONAL. The URL of the API for twitter.com. This defaults to "http://twitter.com/statuses" if not set.

=item apihost

=item apirealm

Optional.
If you do point to a different URL, you will also need to set "apihost" and "apirealm" so that the internal LWP can authenticate.

    "apihost" defaults to "www.twitter.com:80".
    "apirealm" defaults to "Twitter API".

=item templatize
Optional.
A filename of use Template-Toolkit to message formatting. Defaults to 0 (nothing).

=back

=head1 AUTHOR

id774

=head1 SEE ALSO

L<Plagger>, L<Net::Twitter>

=cut
