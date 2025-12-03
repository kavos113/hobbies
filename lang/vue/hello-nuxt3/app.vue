<script
  setup
  lang="ts"
>
import type { Member } from "~/interfaces";

useState<Map<number, Member>>(
    "memberList",
    (): Map<number, Member> => {
        const memberListInit = new Map<number, Member>();
        memberListInit.set(33456, {
            id:     33456,
            name:   "田中太郎",
            email:  "bow@example.com",
            points: 100,
            note:   "初回入会特典あり"
        });
        memberListInit.set(33457, {
            id:     33457,
            name:   "山田花子",
            email:  "woe@example.com",
            points: 200
        });
        
        return memberListInit;
    }
);
</script>

<template>
  <header>
    <h1>ルーティングサンプル</h1>
  </header>
  <main>
    <NuxtPage/>
  </main>
</template>

<style>
main {
    border: solid 1px blue;
    padding: 10px;
}

#breadcrumbs ul li {
    display: inline;
    list-style-type: none;
}

#breadcrumbs {
    margin-left: 0;
}

#breadcrumbs ul {
    padding-left: 0;
}

#breadcrumbs ul .current {
    color: red;
}

#breadcrumbs ul li:before {
    content: " > ";
}

#breadcrumbs ul li:first-child:before {
    content: none;
}
</style>