if [ $(pgrep -f "conky/brightness.conf" | wc -w) -eq 0 ]; then
	conky -c ~/.config/conky/brightness.conf
fi
