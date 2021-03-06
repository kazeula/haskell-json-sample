# haskell-json-sample

This small project is made to explain Haskell to my co-worker.

Most comments are written in Japanese.

## 概要

このプロジェクトはlivedoorが提供する天気APIを用いて、
指定地点の天気予報を取得するライブラリを提供する。

また、ライブラリの利用例として東北の天気予報を
取得してファイルに書き出すアプリケーションを提供する。

## 構成

```
├── src                       # 天気APIを使うためのライブラリ
│   ├── WeatherTool
│   │   └─── JsonScheme.hs # 天気APIが返すJSONの定義
│   └── WeatherTool.hs       # ライブラリ本体
├── app                       # ライブラリを使うアプリケーション
│   └── Main.hs
├── test                      # テスト(未実装)
│   ├── Spec.hs              # ユニットテスト
│   └── doctest.hs           # ドキュメントテスト
├── haskell-json-sample.cabal # pom.xmlみたいなやつ
└── stack.yaml                # Stackの設定(自動生成のまま)
```

このプロジェクトは説明のためのプロジェクトなので、上記に加えて
Haddockで生成されたドキュメントを同梱している。

* `.stack-work/install/x86_64-osx/lts-2.15/7.8.4/doc/index.html`

## ビルド方法

このプロジェクトはStackを利用して開発している。
StackはHaskellのワークアラウンド
(依存解決、ビルド、テスト、ドキュメント生成等)
を便利にしてくれるツールである。
特にStackageによってDependency Hell問題を解決してくれるところが嬉しい。

ビルドにはまず Stack(0.1.2) を用意する。
Stackを動かすためには GHC-7.8.4 と cabal-install が必要である。
個々に導入するか、もしくは Haskell-Platform(GHC-7.8.4) を導入する。

Stackがひとたび動けば、適切なGHCバージョンを
準備することすらStackがやってくれる。

ビルドには以下のコマンドを実行する。
```
$ stack build
```

ビルドしたものは以下のコマンドで動かせる。
```
$ stack exec haskell-json-sample-exe
```

テストはまだ用意していないが、テストがあればは以下のコマンドで実行できる。
```
$ stack test
```

HaddockはHaskellでいうJavadocみたいなものである。
今回は説明のため、自作ライブラリ分のHaddockはリポジトリに含めてしまっている。
Haddockは以下のコマンドで生成できる。
```
$ stack haddock --no-haddock-deps # 外部ライブラリのHaddockは生成しない
$ stack haddock                   # 外部ライブラリのHaddockも生成
```

## 実行例

`stack exec haskell-json-sample-exe`を実行すると以下の内容の
ファイル(tmp.txt)が生成される。

```
020010: 青森県 青森 の天気
        今日 2015-08-17 曇のち雨
        明日 2015-08-18 曇時々雨

050010: 秋田県 秋田 の天気
        今日 2015-08-17 曇のち雨
        明日 2015-08-18 曇時々雨

999999: *** Failure. ***

030010: 岩手県 盛岡 の天気
        今日 2015-08-17 曇のち雨
        明日 2015-08-18 曇時々雨

060010: 山形県 山形 の天気
        今日 2015-08-17 曇のち雨
        明日 2015-08-18 曇り

040010: 宮城県 仙台 の天気
        今日 2015-08-17 曇のち雨
        明日 2015-08-18 曇時々雨

070010: 福島県 福島 の天気
        今日 2015-08-17 雨時々曇
        明日 2015-08-18 曇り
```

## 使用している主なライブラリ

具体的な使用方法はソースコードのコメントを参照されたい。

* Lens
  * データ構造へのアクセス(ゲッター、セッターなど)を超簡単に行うためのライブラリ。
* Wreq
  * HTTPリクエストを行うためのライブラリ。レスポンスのデータ構造はLensでアクセスされるように作られる。
* Conduit
  * HaskellのLazyなIOはリソース管理などの面で問題がある。その辺りを解決するためのライブラリ。

