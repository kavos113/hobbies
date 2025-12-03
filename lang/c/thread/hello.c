#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_THREADS 5

void *thread_func(void *arg)
{
    int id = (int)arg;
    for (int i = 0; i < 5; i++)
    {
        printf("Thread %d: %d\n", id, i);
        sleep(1);
    }

    return "Thread finished";
}

int main(int argc, char **argv)
{
    pthread_t threads[NUM_THREADS];
    int rc;
    int i;

    for (i = 0; i < NUM_THREADS; i++)
    {
        printf("Main: creating thread %d\n", i);
        rc = pthread_create(&threads[i], NULL, thread_func, (void *)i);
        if (rc)
        {
            printf("Error: return code from pthread_create() is %d\n", rc);
            exit(-1);
        }
    }

    for (i = 0; i < NUM_THREADS; i++)
    {
        void *status;
        rc = pthread_join(threads[i], &status);
        if (rc)
        {
            printf("Error: return code from pthread_join() is %d\n", rc);
            exit(-1);
        }
        printf("Main: completed join with thread %d having a status of %s\n", i, (char *)status);
    }

    return 0;
}