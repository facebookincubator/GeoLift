/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';


const features = [
  {
    title: 'Omni-Channel Efficiency with Hudsons Bay Company',
    imageUrl: 'https://external.fmex11-1.fna.fbcdn.net/safe_image.php?d=AQFjquuBqfvq1Of9&url=https%3A%2F%2Flookaside.fbsbx.com%2Felementpath%2Fmedia%2F%3Fmedia_id%3D166438408504332%26version%3D1631554000&_nc_eui2=AeHHUZGWrSGlEkNzxALwQ2keyNAnOYS1j0rI0Cc5hLWPSgbeXktw9n7LIWPx9qtPPi8&_nc_oe=6ecba&_nc_sid=62ba52&ccb=3-5&gt=1&_nc_hash=AQEECDL_eSX4Egkf',
    description: (
      <>
        Using GeoLift, Hudson’s Bay was able to measure lift in online and in-store sales
        as a result of campaigns on Facebook and Instagram. The company saw a 12.9X incremental
        return on ad spend (iROAS) utilizing this approach.
        <a href="https://www.facebook.com/business/measurement/case-studies/hudsons-bay-company#">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Omni-Channel Measurement for Scotiabank',
    imageUrl: 'https://scontent.xx.fbcdn.net/v/t1.6435-1/p100x100/68609517_2434401649968958_7382717244708487168_n.png?_nc_cat=1&ccb=1-5&_nc_sid=dbb9e7&_nc_eui2=AeEwsl7pDmqqQ5-xwricpZ4vpVCZKap5z9WlUJkpqnnP1e5ksjdNwaHe6UHLlbo_uNY&_nc_ohc=zs8YnsN9T_sAX-vOSQd&_nc_ht=scontent.xx&oh=0762a6d60f2500a6b9949fb34dc53260&oe=617D9EE8',
    description: (
      <>
        Scotiabank Mexico was able to measure for the first time the omni-channel
        incremental impact that their Facebook campaigns had on bank account openings.
        <a href="https://www.facebook.com/business/success/2-scotiabank-mexico">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: '',
    imageUrl: '',
    description: (
      <>

      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{"GeoLift Success Cases"}</h1>
          <div className={styles.buttons}>
          </div>
        </div>
      </header>
      <main>
        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
