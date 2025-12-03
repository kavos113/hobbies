import { Post } from '../src/models/Post';

describe('Post Model', () => {
  it('新しい投稿を作成できること', () => {
    const post = new Post({
      title: 'テスト投稿',
      content: 'これはテスト投稿です。',
      authorId: '1'
    });

    expect(post.title).toBe('テスト投稿');
    expect(post.content).toBe('これはテスト投稿です。');
    expect(post.authorId).toBe('1');
    expect(post.id).toBeDefined();
    expect(post.createdAt).toBeInstanceOf(Date);
  });

  it('必須フィールドが欠けている場合はエラーになること', () => {
    expect(() => {
      new Post({
        title: '',
        content: 'コンテンツ',
        authorId: '1'
      });
    }).toThrow('タイトルは必須です');

    expect(() => {
      new Post({
        title: 'タイトル',
        content: '',
        authorId: '1'
      });
    }).toThrow('コンテンツは必須です');
  });
});
