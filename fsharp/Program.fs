open System
open System.IO
open FSharp.NativeInterop
open FSharp.Json
open System.Collections.Generic

#nowarn "9"

let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
    let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
    Span<'a>(p, length)


[<Struct>]
type Post =
    { _id: string
      title: string
      tags: string[] }

[<Struct>]
type RelatedPosts =
    { _id: string
      tags: string[]
      related: Post[] }

let srcDir = __SOURCE_DIRECTORY__
let posts = Json.deserialize<Post[]> (File.ReadAllText $"{srcDir}/../posts.json")

let stopwatch = Diagnostics.Stopwatch()
stopwatch.Start()

// Start work
let tagPostsTmp = Dictionary<string, Stack<int>>()

posts
|> Array.iteri (fun postId post ->

    for tag in post.tags do

        match tagPostsTmp.TryGetValue tag with
        | true, s -> s.Push postId
        | false, _ ->
            let newStack = Stack()
            newStack.Push postId
            tagPostsTmp[tag] <- newStack)

// convert from Dict<_,Stack<int>> to Dict<_,int[]> for faster access
let tagPosts = Dictionary(tagPostsTmp.Count)

for kv in tagPostsTmp do
    tagPosts[kv.Key] <- kv.Value.ToArray()

[<Literal>]
let topN = 5

let allRelatedPosts: RelatedPosts[] =
    posts
    |> Array.mapi (fun postId post ->
        let taggedPostCount = stackalloc posts.Length
        let top5TagCounts = stackalloc topN
        let top5Posts = Array.zeroCreate topN

        for tagId in post.tags do
            for relatedPostId in tagPosts[tagId] do
                taggedPostCount[relatedPostId] <- taggedPostCount[relatedPostId] + 1

        taggedPostCount[postId] <- 0 // ignore self

        for relatedPostId in 0 .. taggedPostCount.Length - 1 do
            let relatedPostTagCount = taggedPostCount[relatedPostId]

            // Check that this Post is in the Top 5 Counts
            if relatedPostTagCount > top5TagCounts[4] then

                // Find the insertion point for the new Post
                let mutable insertionPoint = 5

                while insertionPoint > 0 &&
                      relatedPostTagCount > top5TagCounts[insertionPoint - 1] do
                        insertionPoint <- insertionPoint - 1

                // Shuffle posts down
                let mutable shuffleIndex = 4

                while shuffleIndex < insertionPoint do
                    top5TagCounts[shuffleIndex] <- top5TagCounts[shuffleIndex - 1]
                    top5Posts[shuffleIndex] <- top5Posts[shuffleIndex - 1]
                    shuffleIndex <- shuffleIndex - 1

                top5TagCounts[insertionPoint] <- relatedPostTagCount
                top5Posts[insertionPoint] <- posts[relatedPostId]


        { _id = post._id
          tags = post.tags
          related = top5Posts }

    )


stopwatch.Stop()
printfn "Processing time (w/o IO): %dms" stopwatch.ElapsedMilliseconds
let json = Json.serialize allRelatedPosts

File.WriteAllText($"{srcDir}/../related_posts_fsharp.json", json)
