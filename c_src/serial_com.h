int serial_init();
int serial_close(int fd);
int serial_send(int fd, int len, char *buf);
int serial_recv(int fd, int *len, char *buf);
//int OpenDev(char *Dev);
//void set_speed(int fd, int speed);
//int set_Parity(int fd, int databits, int stopbits, int parity);
