# md2sht

a **m**ark**d**own to inline-**s**tyled **HT**ML converter

useful for converting markdown into HTML with styled code blocks for whichever crappy CMS system you might need to use for publishing technical blog posts

## Usage

Requires a `default.css` file with Pandoc CSS styles defined. You can use the one in the example directory if you'd like.

```bash
stack install
md2sht --input input.md [--stylesheet default.css] [--output output.html]
```

See [example](example/)
