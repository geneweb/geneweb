RM=/bin/rm -f

all out opt: LICENSE.htm

clean:
	$(RM) -f LICENSE.htm

LICENSE.htm: ../LICENSE
	(echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\""; \
	 echo "            \"http://www.w3.org/TR/REC-html40/loose.dtd\">"; \
	 echo "<head><title>License</title></head><body><pre>"; \
	 cat ../LICENSE; echo "</pre></body>") > LICENSE.htm
