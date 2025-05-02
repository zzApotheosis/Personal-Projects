#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/ktime.h>
#include <asm/uaccess.h>

#define DEVICE_NAME "lkm2"
#define EXAMPLE_MSG "Hello, world!\n"
#define MSG_BUFFER_LEN 15

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Steven C. Jennings");
MODULE_DESCRIPTION("A simple Linux kernel module example.");
MODULE_VERSION("0.01");

extern unsigned int zig_increment(const unsigned int);
extern void init_rando(const long);
extern unsigned int gimme_rando(void);

static int device_open(struct inode * const, struct file * const);
static int device_release(struct inode * const, struct file * const);
static ssize_t device_read(struct file * const, char *, size_t, loff_t * const);
static ssize_t device_write(struct file * const, const char * const, const size_t, loff_t * const);

static int major_num = 0;
static int device_open_count = 0;
static char msg_buffer[MSG_BUFFER_LEN];
static char * msg_ptr = 0;

static struct file_operations file_ops = {
  .read = device_read,
  .write = device_write,
  .open = device_open,
  .release = device_release
};

static ssize_t device_read(struct file * const flip, char * buffer, size_t len, loff_t * const offset) {
  int bytes_read = 0;
  if (*msg_ptr == 0)
    msg_ptr = msg_buffer;
  while (len && *msg_ptr) {
    put_user(*(msg_ptr++), buffer++);
    len--;
    bytes_read++;
  }
  
  return bytes_read;
}

static ssize_t device_write(struct file * const flip, const char * const buffer, const size_t len, loff_t * const offset) {
  printk(KERN_WARNING "This operation is not supported.\n");
  unsigned int data = 0;
  data = zig_increment(data);
  printk(KERN_INFO "Data = %d\n", data);
  data = gimme_rando();
  printk(KERN_INFO "Random number = %d\n", data);

  return -EINVAL;
}

static int device_open(struct inode * const inode, struct file * const file) {
  if (device_open_count)
    return -EBUSY;
  device_open_count++;
  try_module_get(THIS_MODULE);
  return 0;
}

static int device_release(struct inode * const inode, struct file * const file) {
  device_open_count--;
  module_put(THIS_MODULE);
  return 0;
}

static int __init lkm_example_init(void) {
  strncpy(msg_buffer, EXAMPLE_MSG, MSG_BUFFER_LEN);
  msg_ptr = msg_buffer;

  long random_number = 0;
  get_random_bytes(&random_number, sizeof(long));
  //init_rando(random_number);

  s64 uptime_ms = ktime_to_ms(ktime_get_boottime());
  init_rando(uptime_ms);
  

  major_num = register_chrdev(0, DEVICE_NAME, &file_ops);
  if (major_num < 0) {
    printk(KERN_ALERT "Could not register device: %d\n", major_num);
    return major_num;
  } else {
    printk(KERN_INFO "lkm2 module loaded with device major number %d\n", major_num);
    return 0;
  }
}

static void __exit lkm_example_exit(void) {
  unregister_chrdev(major_num, DEVICE_NAME);
  printk(KERN_INFO "Goodbye, world!\n");
}

module_init(lkm_example_init);
module_exit(lkm_example_exit);

