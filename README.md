# DistributedSystems

##Lab 1 Instructions
The [echo.php](https://github.com/pgeogheg/DistributedSystems/blob/master/Lab1/echo.php) remains unchanged from the original file on the website.

The [client.py](https://github.com/pgeogheg/DistributedSystems/blob/master/Lab1/client.py) is the client that connects to the server using sockets. The client connects to localhost, but I had to change this
when running the client and server on different virtual machines. 'localhost' was replaced with the IP address of the server node.

The client communicates by connecting to the socket and sending the message "moo" to the server. The message is wrapped in a HTML request.
The server reads the message, capitalises it and returns it to the client. The client then prints out the message it received back from the
server (i.e. "Moo").

To run the client script, type "python client.py" into the console.
