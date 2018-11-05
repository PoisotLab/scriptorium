test.pdf: test.md
	pandoc $< -o $@ \
		--pdf-engine=weasyprint \
		--webtex \
		--filter pandoc-fignos \
		--filter pandoc-tablenos \
		--filter pandoc-eqnos \
		--bibliography references.json \
		--template=manuscript.html \
		--css styles/preprint.css
