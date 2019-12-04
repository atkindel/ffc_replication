# Fragile Families Challenge paper replication materials

Replication materials for:
- "Measuring the predictability of life outcomes using a scientific mass collaboration"
- "Introduction to the Special Collection on the Fragile Families Challenge" 

## Reproducing all results

The following shell commands provide a basic interface to reproduce all figures in the paper:

```
# In top-level directory of ffc_replication/
# Build image and start a container with it
docker build -t ffc_replication .
docker run -it -d --name ffc ffc_replication

# Copy private data directory into our new container
docker cp ./data/private ffc:/ffc_replication/data/

# Run all reproduction code from inside container
docker exec -it ffc /bin/bash
# Not run (execute these in container shell):
# cd ffc_replication/code/
# source run.sh

# Destroy container when we're finished (not run)
# docker stop ffc
# docker rm ffc
```

You may also need to increase Docker's memory limit in order to generate some of the figures. The heatmaps in Fig. 3 and Fig. S2a are especially memory-intensive.