package minio

val api: Signature & Synchronization & Timing = 
        new Simple 
        with Fibers 
        with Timing 
        with Synchronization {}