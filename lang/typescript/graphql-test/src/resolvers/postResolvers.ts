import { Post } from '../models/Post';

// インメモリストレージ
const posts: Post[] = [];

export const postResolvers = {
  Query: {
    posts: () => posts,
    post: (_: any, { id }: { id: string }) => {
      return posts.find(post => post.id === id);
    }
  },

  Mutation: {
    createPost: (_: any, { input }: { input: { title: string; content: string; authorId: string } }) => {
      const post = new Post(input);
      posts.push(post);
      return post;
    }
  },

  Post: {
    createdAt: (post: Post) => post.createdAt.toISOString()
  }
};
