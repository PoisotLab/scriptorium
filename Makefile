test.pdf: test.md
	pandoc $< -o $@ \
		--pdf-engine=weasyprint \
		--mathml \
		--filter pandoc-fignos \
		--filter pandoc-tablenos \
		--filter pandoc-eqnos \
		--bibliography references.json \
		--template=manuscript.html \
		--css styles/preprint.css
