kojiki=03gendaigoyaku_kojiki.txt

all: ${kojiki}.sjis ${kojiki}.eucjp ${kojiki}.jis ${kojiki}.utf8 ${kojiki}.utf16
	rm ${kojiki}

${kojiki}:
	wget -q http://www.aozora.gr.jp/cards/001518/files/51732_ruby_44617.zip
	mv 51732_ruby_44617.zip kojiki.zip
	unzip kojiki.zip -d .
	rm kojiki.zip

${kojiki}.sjis: ${kojiki}
	cp $< $@

${kojiki}.eucjp: ${kojiki}
	iconv -f SJIS -t EUC-JP -o $@ $<

${kojiki}.jis: ${kojiki}
	iconv -f SJIS -t ISO2022JP -o $@ $<

${kojiki}.utf8: ${kojiki}
	iconv -f SJIS -t UTF-8 -o $@ $<

${kojiki}.utf16: ${kojiki}
	iconv -f SJIS -t UTF-16 -o $@ $<
