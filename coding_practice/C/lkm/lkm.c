#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Steven C. Jennings");
MODULE_DESCRIPTION("A simple Linux module example.");
MODULE_VERSION("0.01");

static int __init lkm_init(void) {
    printk(KERN_INFO "Hello, World!\n");
    return 0;
}

static void __exit lkm_exit(void) {
    printk(KERN_INFO "Goodbye, World!\n");
}

module_init(lkm_init);
module_exit(lkm_exit);

