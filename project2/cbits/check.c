#include <libtar.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

void check() {
	int ret;
	TAR *t;

	ret = tar_open(&t, "example.tar", NULL, O_RDONLY, 0, TAR_VERBOSE);
	if (ret == -1) {
		fprintf(stderr, "tar_open failed\n");
		exit(1);
	}
	printf("Successfully opened example.tar\n");

	ret = tar_close(t);
	if (ret == -1) {
		fprintf(stderr, "tar_close failed\n");
		exit(1);
	}
	printf("Successfully closed example.tar\n");
}
