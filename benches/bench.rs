#![feature(test)]

extern crate test;

#[macro_use] extern crate fomat_macros;

use test::{Bencher, black_box};

#[bench]
fn literal_fomat(b: &mut Bencher) {
    b.iter(|| fomat!("Hello"))
}

#[bench]
fn literal_format(b: &mut Bencher) {
    b.iter(|| format!("Hello"))
}

#[bench]
fn short_fomat(b: &mut Bencher) {
    let world = "world";
    b.iter(|| fomat!("Hello, "(black_box(world))"!") )
}

#[bench]
fn short_format(b: &mut Bencher) {
    let world = "world";
    b.iter(|| format!("Hello, {}!", black_box(world)) )
}

#[bench]
fn long_fomat(b: &mut Bencher) {
    let world = "world";
    b.iter(|| fomat!(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In convallis
        ligula nibh, at maximus diam eleifend sit amet. Suspendisse mattis
        nisi quis eleifend commodo. Praesent ut dui luctus, ullamcorper felis
        et, dictum purus. Nullam elit nunc, dictum ornare arcu vitae, suscipit
        pulvinar felis. Donec malesuada sollicitudin arcu, sit amet laoreet
        dolor elementum eu. Cras quam augue, feugiat ac imperdiet vel, posuere
        et elit. Nunc consequat bibendum dolor at auctor. Cras vel urna
        fermentum, viverra est ut, posuere elit. Suspendisse mattis aliquam
        tincidunt.

        Pellentesque ac risus eu nunc molestie cursus id in risus.
        Pellentesque vel ipsum sed lectus vehicula fermentum vitae vel est.
        Phasellus suscipit dolor eu finibus tincidunt. Duis interdum lectus at
        interdum viverra. Phasellus in mi dapibus, condimentum ligula at,
        rhoncus quam. Morbi pulvinar tortor sit amet fringilla sodales. Duis
        feugiat felis ut ante efficitur, vitae consectetur leo luctus. Vivamus
        congue, ex vel ullamcorper auctor, quam dolor vulputate purus, quis
        dapibus nulla ipsum in ligula. Sed sed vehicula odio. Quisque bibendum
        efficitur sodales. In aliquet sollicitudin venenatis. Sed id euismod
        nulla. Cras risus ligula, mattis in arcu eu, pulvinar aliquet metus.
        Mauris et convallis eros, in tempus sem."
        (world)(1)(2)(3)[vec![4]] "!"
    ) )
}

#[bench]
fn long_format(b: &mut Bencher) {
    let world = "world";
    b.iter(|| format!(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In convallis
        ligula nibh, at maximus diam eleifend sit amet. Suspendisse mattis
        nisi quis eleifend commodo. Praesent ut dui luctus, ullamcorper felis
        et, dictum purus. Nullam elit nunc, dictum ornare arcu vitae, suscipit
        pulvinar felis. Donec malesuada sollicitudin arcu, sit amet laoreet
        dolor elementum eu. Cras quam augue, feugiat ac imperdiet vel, posuere
        et elit. Nunc consequat bibendum dolor at auctor. Cras vel urna
        fermentum, viverra est ut, posuere elit. Suspendisse mattis aliquam
        tincidunt.

        Pellentesque ac risus eu nunc molestie cursus id in risus.
        Pellentesque vel ipsum sed lectus vehicula fermentum vitae vel est.
        Phasellus suscipit dolor eu finibus tincidunt. Duis interdum lectus at
        interdum viverra. Phasellus in mi dapibus, condimentum ligula at,
        rhoncus quam. Morbi pulvinar tortor sit amet fringilla sodales. Duis
        feugiat felis ut ante efficitur, vitae consectetur leo luctus. Vivamus
        congue, ex vel ullamcorper auctor, quam dolor vulputate purus, quis
        dapibus nulla ipsum in ligula. Sed sed vehicula odio. Quisque bibendum
        efficitur sodales. In aliquet sollicitudin venenatis. Sed id euismod
        nulla. Cras risus ligula, mattis in arcu eu, pulvinar aliquet metus.
        Mauris et convallis eros, in tempus sem.{}{}{}{}{:?}!",
        world, 1, 2, 3, vec![4]
    ) )
}
