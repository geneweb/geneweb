all opt: LICENSE.htm

clean:
	$(RM) LICENSE.htm

LICENSE.htm: ../LICENSE
	(echo "<head><title>Licence</title></head><body><pre>"; \
	 cat ../LICENSE; echo "</pre></body>") > LICENSE.htm
