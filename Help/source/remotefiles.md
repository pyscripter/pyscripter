:::{index} Remote Files
:::

# Working with Remote Files

Pyscripter supports working with remote files i.e. files that may reside in different 
computers (servers) including Windows and Linux machines. You can open, edit run debug 
 and save back these files. They work seamlessly with other PyScripter 
 features such as [the Recent File list](filemenu), 
 [project files](projectexplorer), and [Run Configurations](runconfigurations).


### Requirements

To use PyScripter with remote files your computer need to have SSH 
client capabilities at the computer running PyScripter and an SSH server 
running on the remote computer. SSH is a widely used network protocol 
for securely connecting to remote machines. Windows 10 since the April 
2018 update includes SSH. With earlier versions of Windows 10 you need to 
manually enable SSH through "Enable Optional Features". For 
other versions of Windows you can install the latest version of
[OpenSSH for Windows](https://github.com/PowerShell/Win32-OpenSSH) 
using the provided 
[installation instructions](https://github.com/PowerShell/Win32-OpenSSH/wiki/Install-Win32-OpenSSH).  
  
Alternatively on the client side you can use the popular SSH client
[PuTTY](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html).  
  

#### Configuring the SSH client side

 Pyscripter requires password-less authentication using rsa keys. You 
 need to create the rsa keys and add them to the ssh-agent service which 
 needs to be running. Your public key needs to be added to the 
 ~/.ssh/authorised\_keys file on the server side. Instructions are 
 provided [here](https://github.com/PowerShell/Win32-OpenSSH/wiki/ssh.exe-examples).


#### Configuring the SSH server side

The SSH server service (sshd) and SSH agent service (ssh-agent) need to be running 
on the server side. This is most likely true for Linux machines. In Windows 
 machines you need to start the server using the commans:

```winbatch
net 
start sshd  
net start ssh-agent  
```

You can also configure these services to run automatically at login time.


#### Testing the SSH connection

From a command prompt issue the following command: 
 
```ssh username@hostname```

where username is the user name on the server side and host name is the IP address of the 
SSH server. If this works and you see the server shell, then PyScripter is ready to use the Server.  


:::{index} Remote Files; Open
:::
### Opening remote files

You can open remote files using the [File Menu](filemenu). You are then shown the Open Remote File dialog shown below:  

![graphic](images/remotefiledialog.JPG){align=center width="27.27em" height="10.02em"}
  
In this dialog box you provide the  path to the remote file and select an SSH server from a drop-down list.  You can also setup your SSH servers by pressing the button next to the SSH 
server field. In PyScripter remote file names are shown in the  UNC format \\\\servername\filepath.  

:::{index} SSH; Setup Server
:::
#### Setting up SSH servers

![graphic](images/SSHserversdialog.JPG){align=center width="18.975em" height="18.60em"}

In this dialog box you add remove or modify SSH servers.   


### Editing SSH server information

![graphic](images/editsshserverdialog.JPG){align=center width="37.15em" height="24.40em"}
  
For each SSH server you need to provide a Name that will be used to identify 
the SSH server, as well as the user name and host name (or IP address) that 
will be used to connect to the server. You also need to provide the path to 
the scp and ssh commads and  the command that will be used to execute Python 
on the server . Optionally you can provide additional ssh and scp -o  options that will 
be passed to the ssh and scp commands. If you want to use password authentication (only with Putty - see below) you also need to check the Password Needed option.

The image above shows default settings for connecting to a Linux SSH server using 
[OpenSSH for Windows](https://github.com/PowerShell/Win32-OpenSSH). 

***NOTE:*** If you are using OpenSSH v9.5 or later that first came with Windows 11 24H2
you need to add the `-O` flag to the scp options or otherwise scp will fail.

Instead of OpenSSH you can use 
[PuTTY](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)
as the SSH client. See below a typical PuTTY configuration of 
an SSH server. The example uses password authentication, but you can 
use a private/public key combination instead for password-free 
authentication. In that case you need to use puttygen to create 
the private/public key and add -i path\_to\_your\_private\_key to the scp 
and ssh options or instead run pagent and add to it the private key. 
You also need to setup your server to accept the public key by appending it to 
the ~/.ssh/authorized\_keys file. See
 [here](https://www.ssh.com/ssh/authorized_keys/openssh) for details.  

![graphic](images/editsshserverdialog_putty.JPG){align=center width="37.15em" height="24.40em"}
