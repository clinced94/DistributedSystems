import socket
import sys

sock = socket.create_connection(('localhost', 8000))

try:
	message = "GET /echo.php?message=moo HTTP/1.0\r\nHost:localhost:8000\r\n\r\n"
	sock.send(message)

	data = sock.recv(1024)
	while data != "":
		print data
		data = sock.recv(1024)

finally:
	sock.close