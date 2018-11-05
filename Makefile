MS=manuscript

$(MS).pdf: $(MS).md
	pandoc $< -o $@ \
		--pdf-engine=weasyprint \
		--webtex \
		--filter pandoc-fignos \
		--filter pandoc-tablenos \
		--filter pandoc-eqnos \
		--bibliography references.json \
		--csl build/plab.csl \
		--template=build/manuscript.html \
		--css styles/preprint.css
