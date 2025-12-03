interface PostInput {
  title: string;
  content: string;
  authorId: string;
}

export class Post {
  id: string;
  title: string;
  content: string;
  authorId: string;
  createdAt: Date;

  constructor(input: PostInput) {
    this.validateInput(input);
    
    this.id = this.generateId();
    this.title = input.title;
    this.content = input.content;
    this.authorId = input.authorId;
    this.createdAt = new Date();
  }

  private validateInput(input: PostInput): void {
    if (!input.title.trim()) {
      throw new Error('タイトルは必須です');
    }
    if (!input.content.trim()) {
      throw new Error('コンテンツは必須です');
    }
  }

  private generateId(): string {
    return Math.random().toString(36).substring(2) + Date.now().toString(36);
  }
}
