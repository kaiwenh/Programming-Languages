

for s in "Alford" "Bolden" "Hamilton" "Parker" "Welsh"
do
    echo Server $s started
    python proxyherd.py $s &
done

