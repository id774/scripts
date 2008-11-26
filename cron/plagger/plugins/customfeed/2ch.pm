package Plagger::Plugin::CustomFeed::2ch;
use strict;
use base qw( Plagger::Plugin );

use DateTime::Format::Strptime;
use Encode;
use Time::HiRes;
use WWW::2ch;

use Plagger::UserAgent;

sub register {
    my($self, $context) = @_;
    $context->register_hook(
        $self,
        'subscription.load' => \&load,
    );
}

sub load {
    my($self, $context) = @_;

    unless ($self->conf->{urls} && $self->conf->{cache}) {
	$context->log(error => "plase config.");
    }

    for (@{ $self->conf->{urls} }) {
	my $feed = Plagger::Feed->new;
	$feed->type('2ch');
	$feed->meta($_);
  $feed->aggregator( sub { $self->aggregate(@_) } );
	$context->subscription->add($feed);
    }
}

sub aggregate {
    my($self, $context, $args) = @_;

    my $feed = $args->{feed};
    my $bbs = WWW::2ch->new(url => $feed->meta->{url},
			    cache => $self->conf->{cache},
			    ua => Plagger::UserAgent->new->agent,
			    plugin => $feed->meta->{plugin});

    if ($bbs->conf->{key}) {
	$self->load_dat($context, $bbs);
    } else {
	$self->load_subject($context, $bbs);
    }
    Time::HiRes::sleep( $self->conf->{fetch_body_interval} || 5 );
}

sub load_dat {
    my ($self, $context, $bbs) = @_;

    my $key = $bbs->conf->{key};
    my $dat = $bbs->recall_dat($key);
    my $last = 0;
    unless (scalar(@{ $dat->reslist })) {
	$bbs->load_setting;
	$bbs->load_subject;
	$dat = $bbs->subject->thread($key);
    } else {
	$last = scalar(@{ $dat->reslist });
    }
    $dat->load;
    $context->log(info => $dat->permalink . ' load succeed.');

    my $encoding = $bbs->worker->encoding;
    my $feed = Plagger::Feed->new;
    $feed->type('2ch');
    $feed->title( decode($encoding, $dat->title) );
    $feed->link($dat->permalink);
    my $format = DateTime::Format::Strptime->new(pattern => '%Y-%m-%d %H:%M:%S');

    my @reslist = @{ $dat->reslist };
    for my $res (@reslist[$last...$#reslist]) {
        my $entry = Plagger::Entry->new;
        $entry->title($res->num);
        $entry->link($dat->permalink);
        $entry->author( decode($encoding, $res->name) );
	my $body = decode($encoding, $res->body);
	$body =~ s!<br>!<br />!g;
	$entry->body($body);

	my $dt = DateTime->from_epoch( epoch => $res->time );
	$dt->set_time_zone($context->conf->{timezone});
	$entry->date( Plagger::Date->parse($format, $dt->ymd . ' ' . $dt->hms) );

        $feed->add_entry($entry);
    }
    $context->update->add($feed) if $feed->count;
}

sub load_subject {
    my ($self, $context, $bbs) = @_;

    $bbs->load_setting;
    $bbs->load_subject;

    my $encoding = $bbs->worker->encoding;
    my $feed = Plagger::Feed->new;
    $feed->type('2ch');
    $feed->title( decode($encoding, $bbs->setting->title) );
    $feed->link($bbs->subject->permalink);

    $context->log(info => $bbs->conf->{subject} . ' load succeed.');

    my $i = 0;
    my $items = $self->conf->{fetch_items} || 50;
    my $format = DateTime::Format::Strptime->new(pattern => '%Y-%m-%d %H:%M:%S');
    for my $dat ($bbs->subject->threads) {
	next if $dat->get_cache;
        last if $i++ >= $items;

	$dat->load;
	my $res = $dat->res(1);

        my $entry = Plagger::Entry->new;
        $entry->title( decode($encoding, $dat->title) );
        $entry->link($dat->permalink);
        $entry->author( decode($encoding, $res->name) );
	my $body = decode($encoding, $res->body);
	$body =~ s!<br>!<br />!g;
	$entry->body($body);

	my $dt = DateTime->from_epoch( epoch => $res->time );
	$dt->set_time_zone($context->conf->{timezone});
	$entry->date( Plagger::Date->parse($format, $dt->ymd . ' ' . $dt->hms) );

        $feed->add_entry($entry);
	$context->log(info => $i . ': load to ' . $dat->permalink . ' succeed.');
	Time::HiRes::sleep( $self->conf->{fetch_body_interval} || 5 );
    }
    $context->update->add($feed) if $feed->count;
}

1;

__END__

=head1 NAME

Plagger::Plugin::CustomFeed::2ch - Custom feed for 2ch

=head1 SYNOPSIS

  - module: CustomFeed::2ch
    config:
      cache: /home/yappo/perl/plagger/2chcache
      fetch_body_interval: 5
      fetch_items: 10
      urls:
       - url: http://live22.2ch.net/news/
       - url: http://pc8.2ch.net/test/read.cgi/php/997829243/
       - url: http://jbbs.livedoor.jp/computer/10298/
         plugin: Jbbs

=head1 DESCRIPTION

This plugin fetches bbs article updates from 2ch
(L<http://2ch.net/>) and creates a custom feed.

=head1 AUTHOR

Kazuhiro Osawa

=head1 SEE ALSO

L<Plagger>, L<WWW::2ch>, L<http://2ch.net/>

=cut
