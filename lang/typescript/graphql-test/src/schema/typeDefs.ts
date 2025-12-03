import { gql } from 'apollo-server';

export const typeDefs = gql`
  type Post {
    id: ID!
    title: String!
    content: String!
    authorId: String!
    createdAt: String!
  }

  input CreatePostInput {
    title: String!
    content: String!
    authorId: String!
  }

  type Query {
    posts: [Post!]!
    post(id: ID!): Post
  }

  type Mutation {
    createPost(input: CreatePostInput!): Post!
  }
`;
