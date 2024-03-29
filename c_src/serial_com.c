#include <stdio.h>      /*标准输入输出定义*/
#include <stdlib.h>     /*标准函数库定义*/
#include <unistd.h>     /*Unix 标准函数定义*/
#include <sys/types.h>  
#include <sys/stat.h>   
#include <fcntl.h>      /*文件控制定义*/
#include <termios.h>    /*PPSIX 终端控制定义*/
#include <errno.h>      /*错误号定义*/
#include <string.h>

#define FALSE -1
#define TRUE 0
#define LENGTH 100

int speed_arr[] = {B115200,B38400,B19200,B9600,B4800,B2400,B1200,B300,B38400,B19200,B9600,B4800,B2400,B1200,B300};

int name_arr[] = {115200,38400,19200,9600,4800,2400,1200,300,38400,19200,9600,4800,2400,1200,300};

int OpenDev(char *Dev);
void set_speed(int fd, int speed);
int set_Parity(int fd, int databits, int stopbits, int parity);

int serial_init()
{
	int fd;
	char *dev  = "/dev/ttyUSB0"; //USB串口
	fd = OpenDev(dev);
	tcflush(fd, TCIFLUSH);
	set_speed(fd,115200);
	if (set_Parity(fd,8,1,'N') == FALSE)  
	{
		printf("Set Parity Error\n");
		exit (0);
	}
//  char send[LENGTH];
//	char recv[LENGTH];
//	fgets(send,LENGTH,stdin);
//	printf("send: %s", send);
//	write(fd, send, LENGTH);
/*	while (1) //循环读取数据
	{   
		while((nread = read(fd,recv,LENGTH)) > 0)
		{
			printf("Len:  %d\n", nread);
			recv[nread] = 0;
			printf("recv:%s\n", recv);
			if (fputs(recv, stdout) == EOF)
				printf("fputs error");
		}
	}*/
//	close(fd);  
	return fd;
}

int serial_close(int fd)
{
		close(fd);
		return 0;
}

int serial_send(int fd, int len, char *buf)
{
		printf("content sending...\n");
		write(fd, buf, len);
		return 0;
}

int serial_recv(int fd, int *len, char *buf)
{
	while(1)
	{
			while((*len = read(fd,buf,LENGTH)) > 0)
			{
					printf("Len: %d\n", *len);
			}
	}
}


/**
 * *@brief  设置串口通信速率
 * *@param  fd     类型 int  打开串口的文件句柄
 * *@param  speed  类型 int  串口速度
 * *@return  void
 * */
void set_speed(int fd, int speed)
{
	int   i; 
	int   status; 
	struct termios   Opt;
	tcgetattr(fd, &Opt); 
	for ( i= 0;  i < sizeof(speed_arr) / sizeof(int);  i++) 
	{ 
	      if  (speed == name_arr[i]) 
	      {     
	            tcflush(fd, TCIOFLUSH);     
	            cfsetispeed(&Opt, speed_arr[i]);  
	            cfsetospeed(&Opt, speed_arr[i]);   
	            status = tcsetattr(fd, TCSANOW, &Opt);  
	            if  (status != 0) 
		    {        
		            perror("tcsetattr fd");  
		            return;     
	            }    
	            tcflush(fd,TCIOFLUSH);   
	      }  
	}
}

/**
 * *@brief   设置串口数据位，停止位和效验位
 * *@param  fd     类型  int  打开的串口文件句柄
 * *@param  databits 类型  int 数据位   取值 为 7 或者8
 * *@param  stopbits 类型  int 停止位   取值为 1 或者2
 * *@param  parity  类型  int  效验类型 取值为N,E,O,S
 * */
int set_Parity(int fd,int databits,int stopbits,int parity)
{ 
	struct termios options; 
	if  ( tcgetattr( fd,&options)  !=  0) 
	{ 
		perror("SetupSerial 1");     
		return(FALSE);  
	}
	options.c_cflag &= ~CSIZE; 
	switch (databits) /*设置数据位数*/
	{   
	case 7:		
		options.c_cflag |= CS7; 
		break;
	case 8:     
		options.c_cflag |= CS8;
		break;   
	default:    
		fprintf(stderr,"Unsupported data size\n"); 
		return (FALSE);  
	}
	switch (parity) 
	{   
	case 'n':
	case 'N':    
		options.c_cflag &= ~PARENB;   /* Clear parity enable */
		options.c_iflag &= ~INPCK;     /* Enable parity checking */ 
		break;  
	case 'o':   
	case 'O':     
		options.c_cflag |= (PARODD | PARENB); /* 设置为奇效验*/  
		options.c_iflag |= INPCK;             /* Disnable parity checking */ 
		break;  
	case 'e':  
	case 'E':   
		options.c_cflag |= PARENB;     /* Enable parity */    
		options.c_cflag &= ~PARODD;   /* 转换为偶效验*/     
		options.c_iflag |= INPCK;       /* Disnable parity checking */
		break;
	case 'S': 
	case 's':  /*as no parity*/   
		options.c_cflag &= ~PARENB;
		options.c_cflag &= ~CSTOPB;break;  
	default:   
		fprintf(stderr,"Unsupported parity\n");    
		return (FALSE);  
	}  
	/* 设置停止位*/  
	switch (stopbits)
	{   
	case 1:    
		options.c_cflag &= ~CSTOPB;  
		break;  
	case 2:    
		options.c_cflag |= CSTOPB;  
		break;
   	default:    
     	        fprintf(stderr,"Unsupported stop bits\n");  
		return (FALSE); 
	} 
	/* Set input parity option */ 
	if (parity != 'n')   
		options.c_iflag |= INPCK; 
	tcflush(fd,TCIFLUSH);
	options.c_cc[VTIME] = 150; /* 设置超时15 seconds*/   
	options.c_cc[VMIN] = 0; /* Update the options and do it NOW */
	if (tcsetattr(fd,TCSANOW,&options) != 0)   
	{ 
		perror("SetupSerial 3");   
		return (FALSE);  
	} 
	return (TRUE);  
}

/*********************************************************************
 * 代码说明：使用usb串口测试的，发送的数据是字符，
 * 但是没有发送字符串结束符号，所以接收到后，后面加上了结束符号。
*********************************************************************/

int OpenDev(char *Dev)
{
	int	fd = open(Dev, O_RDWR);         //| O_NOCTTY | O_NDELAY	
	if (-1 == fd)	
	{ 			
		perror("Can't Open Serial Port");
		return -1;		
	}	
	else	
		return fd;
}

