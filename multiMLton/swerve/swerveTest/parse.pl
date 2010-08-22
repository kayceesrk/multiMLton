#!/usr/bin/perl

$filename = $ARGV[0];
$numConns = $ARGV[1];

print "#Input :: $filename\n";

open(IN, "<$filename");

my $line;

my $total_time = "nil";

my $rate = "nil";

my $connection_rate = "nil";
my $avg_conn = "nil";
my $min_conn = "nil";
my $max_conn = "nil";
my $stddev_conn = "nil";

my $request_rate = "nil";

my $reply_rate = "nil";
my $resp_time = "nil";
my $transfer_time = "nil";

my $io = "nil";

my $errors = "nil";

print "#Rate[conn/s]\tTotal time[ms]\tConnection rate[conn/s]\tRequest rate[replies/s]\tReply rate[replies/s]\tResp time[ms]\tTransfer time[ms]\tAvg conn time[ms]\tMax conn time[ms]\tConn time stddev[ms]\tIO[KBps]\tErrors\n";


while($line = <IN>){

    chomp $line;


    if ($line =~ m/^HTTPERF RATE ([0-9\.]+)/){
        $rate = $1;
    }elsif ( $line =~m/^Total: connections [0-9]+ requests [0-9]+ replies [0-9]+ test-duration ([0-9\.]+) s/ ){
        $total_time = $1;
        $reply_rate = $numConns/$total_time;
    }elsif ( $line =~m/^Connection rate: ([0-9\.]+)/ ){
        $connection_rate = $1;
    }elsif ( $line =~m/^Connection time \[ms\]: min ([0-9\.]+) avg ([0-9\.]+) max ([0-9\.]+) median ([0-9\.]+) stddev ([0-9\.]+)/ ){
        $min_conn=$1;
        $avg_conn=$2;
        $max_conn=$3;
        $stddev_conn=$5;
    }elsif ( $line =~ m/^Request rate: ([0-9\.]+) / ){
        $request_rate = $1;
    }elsif ( $line =~ m/Reply time \[ms\]: response ([0-9\.]+) transfer ([0-9\.]+)/){
        $resp_time = $1;
        $transfer_time = $2;
    }elsif ( $line =~ m/^Net I\/O: ([0-9\.]+) / ){
        $io = $1;
    }elsif ( $line =~m/^Errors: total ([0-9\.]+) / ){
        $errors = $1;
        print "$rate\t$total_time\t$connection_rate\t$request_rate\t$reply_rate\t$resp_time\t$transfer_time\t$avg_conn\t$max_conn\t$stddev_conn\t$io\t$errors\n";
    };
};

;

