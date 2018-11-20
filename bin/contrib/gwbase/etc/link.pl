
#!/usr/bin/perl -w
#help
# /data/src/gwbase/etc/connex.opt -a  ../roglo |perl link.pl |sort -r >connex.txt
#
# Roglo
#$BASE="roglo";
$nbfam = 0;
$nbcnx = 0;
$nbone = 0;
$szbloc = 0;
$nbbloc = 0;
@blocs = (0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0);

while (<>) {
#	next unless (/^\*/);
    if (/^Connex.+length (\d+)/) {
		$nbbloc++;
		$szbloc = $1;
		$nbfam += $1;
		$blocs[$1] ++ if ($1<16);
		$nbone++ if ($1==1);
		$nbcnx = $1 if ($nbcnx < $1);
		chomp;
		$_ = sprintf("\n*%04d ",$1).$_;
	}
	elsif (/^i=(\d+)$/) { $_ = " <a href='%si=$1;'>?$1</a>"; }
	elsif (/\(i=(\d+)\)/) { $_ = " <a href='%si=$1;'>?$1</a>"; }
	elsif (/^  - (.+)\.(\d+) (.+)\n/) { $_ = " [[$1/$3/$2/$1 $3]]"; }
	print;
#if ($szbloc>1);
}
print "\n*Statistiques\n*Résultat : $nbfam familles réparties en $nbbloc blocs.\n";
printf "*Meilleur bloc connexe : $nbcnx. => Connexité : %2.2f%%.\n*Mais $nbone familles restent isolées (%2.2f%%)\n", 100*$nbcnx/$nbfam, 100*$nbone/$nbfam;
print  "*Liste : ";
for ($i=1;$i<16;$i++) { print $blocs[$i]."($i)  "; }
print  "\n";