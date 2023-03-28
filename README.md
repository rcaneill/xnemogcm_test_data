# xnemogcm_test_data

Create NEMO outputs for xnemogcm testing.

The workflow is based on GNU/make and apptainer containers. Hence only 2 dependencies
are necessary to run the whole workflow and produce the nemo outputs. Please check
your linux distribution package manager to install make and apptainer.

To build the containers, and run nemo, run in a shell

```shell
make all
```
